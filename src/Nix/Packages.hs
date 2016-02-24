{-# LANGUAGE NamedFieldPuns, NoImplicitPrelude, OverloadedStrings, ExtendedDefaultRules, RecordWildCards, NoMonomorphismRestriction, TemplateHaskell, QuasiQuotes, RecordWildCards #-}

module Nix.Packages where 

import BasicPrelude hiding (try, (</>), (<.>), FilePath, show, rem, takeWhile)

import qualified Data.Text as Text
import qualified Data.Char as Char
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Lens
import Text.Printf.TH

import Utils 

-- -------------------------------------------------------------------
-- Packages
-- -------------------------------------------------------------------

type PackageName = Text
type PackageVersion = Text
type Package = Text
type StorePath = Text

data VersionedPackage = VPkg { pName :: PackageName
                             , pVer :: PackageVersion 
                             }
  deriving (Eq, Ord, Show)
  
data PackageWithPath = Pwp { pwpPkg :: VersionedPackage
                           , pwpPath :: StorePath
                           }
  deriving (Eq, Ord, Show)

  
data PkgStatus = Present | Prebuilt | Source
  deriving (Eq, Ord, Show)
           
parseVersionedPackage :: Package -> VersionedPackage
parseVersionedPackage p = VPkg{..}
  where (pName, pVer) = over both (Text.intercalate "-") 
                                     (splitFirstNonLetter (Text.splitOn "-" p))
        splitFirstNonLetter :: [Text] -> ([Text],[Text])
        splitFirstNonLetter xs = fromMaybe (xs,[])
                                           (splitAt <$> findIndex firstNonLetter xs <*> pure xs)
        firstNonLetter :: Text -> Bool
        firstNonLetter t = isJust $ guard . not . Char.isLetter =<< preview _head t

  
formatVersionPackage :: VersionedPackage -> PackageName
formatVersionPackage (VPkg {..}) = if Text.null pVer 
                                   then pName 
                                   else pName <> "-" <> pVer

formatPackageWithPath :: PackageWithPath -> PackageName
formatPackageWithPath = formatVersionPackage . pwpPkg


-- -------------------------------------------------------------------
-- Updates
-- -------------------------------------------------------------------
data Upd = Upd { uName :: Package
               , uOld :: PackageVersion 
               , uOldPath :: StorePath
               , uNew :: PackageVersion 
               , uNewPath :: StorePath
               , uStatus :: PkgStatus -- ^ the status of the new package
               }
  deriving (Eq, Ord, Show)
  
formatUpd :: Upd -> Text
formatUpd Upd{uName, uOld, uNew, uStatus} = 
  [st|%s (%s -> %s %?)|] uName uOld uNew uStatus
       
oldPackage, newPackage :: Upd -> PackageWithPath
oldPackage Upd{..} = Pwp { pwpPkg = VPkg{pName = uName, pVer = uOld}
                         , pwpPath = uOldPath }
newPackage Upd{..} = Pwp { pwpPkg = VPkg{pName = uName, pVer = uNew}
                         , pwpPath = uNewPath 
                         -- , pwpStatus = uStatus 
                         }
newPackageAndStatus :: Upd -> (PackageWithPath, PkgStatus)
newPackageAndStatus upd@Upd{..} = (newPackage upd, uStatus)


calculateUpdates :: Map PackageWithPath PkgStatus -- ^ added packages
           -> Set PackageWithPath -- ^ removed packages
           -> (Set Upd, Map PackageWithPath PkgStatus, Set PackageWithPath)
calculateUpdates fresh removed = (upds, fresh', removing')
    where upds = S.fromList $
                 [upd | (f, status) <- M.assocs fresh 
                      , r <- S.toList removed
                      , upd <- maybeToList $ findUpdate (f, status) r
                 ]
          removing' = removed S.\\ S.map oldPackage upds 
          fresh' = removeKeys fresh (S.map newPackage upds)

-- | Determine if an added package and a removed package form an update.
findUpdate :: (PackageWithPath, PkgStatus) -- ^ Added package
           -> PackageWithPath -- ^ Removed package
           -> Maybe Upd
findUpdate added removed = mkUpd added removed
  where mkUpd (Pwp { pwpPkg = (VPkg { pName = uName , pVer = uNew}) 
                   , pwpPath = uNewPath
                   }
               , uStatus) 
              (Pwp { pwpPkg = (VPkg { pName = n2, pVer =  uOld})
                   , pwpPath = uOldPath
                   }) 
                | uName == n2  = Just (Upd {..})
                | otherwise = Nothing
  

data Results = Results { rStorePaths :: Set PackageWithPath
                       , rDeclared :: Map PackageWithPath PkgStatus
                       , rInstalled :: Map PackageWithPath PkgStatus }
  deriving Show

removing,installing :: Results -> Map PackageWithPath PkgStatus
removing r@Results{ rInstalled, rDeclared } = 
  removeKeys (rInstalled M.\\ rDeclared) (rStorePaths r)
installing r@Results{ rInstalled } = 
  (wantedFromDeclared r M.\\ rInstalled) 

-- | declared packages that should be installed, i.e. that are not
-- overriden by some store paths
wantedFromDeclared :: Results -> Map PackageWithPath PkgStatus
wantedFromDeclared r@Results{  rDeclared } = 
  removeKeys rDeclared (S.map newPackage (overridingStorePaths r))

-- | Store paths overriding declared packages (as a set of updates)
overridingStorePaths:: Results -> Set Upd
overridingStorePaths Results{ rStorePaths, rDeclared }  = us
  where (us, _, _) = calculateUpdates rDeclared rStorePaths

-- | Updates that are prevented by store paths overriding declared
-- packages. These are packages that will not change even though there
-- is a newer version in nixpkgs.
blockedUpdates :: Results -> Set Upd
blockedUpdates r@Results{ rInstalled } = S.filter isNonTrivial us
  where (us, _,_) = calculateUpdates 
                      (M.fromList . map newPackageAndStatus . S.toList $ overridingStorePaths r)
                      (M.keysSet rInstalled)

-- | Updates that result from store paths
updatingStorePaths :: Results -> Set Upd
updatingStorePaths Results{ rInstalled, rStorePaths } = S.filter isNonTrivial us
  where (us, _,_) = calculateUpdates (addStorePathStatus rStorePaths) (M.keysSet rInstalled)
        -- TODO: actually rStorePaths could have a proper status from the start
        addStorePathStatus = M.fromSet (const Present) 

isStrictUpdate, isReinstall, isNonTrivial :: Upd -> Bool
isStrictUpdate Upd{uOld, uNew} = uOld /= uNew
isReinstall Upd{..} = uOld == uNew && uOldPath /= uNewPath
isNonTrivial u = isStrictUpdate u || isReinstall u
