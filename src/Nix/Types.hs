{-# LANGUAGE NamedFieldPuns, NoImplicitPrelude, OverloadedStrings, ExtendedDefaultRules, RecordWildCards, NoMonomorphismRestriction, TemplateHaskell, QuasiQuotes, RecordWildCards #-}

module Nix.Types where 

import BasicPrelude hiding (try, (</>), (<.>), FilePath, show, rem, takeWhile)

import qualified Data.Text as Text
import qualified Data.Char as Char
import Formatting ((%))
import qualified Formatting as Fmt
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Lens

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
  Fmt.sformat (""%Fmt.stext%" ("%Fmt.stext%" -> "%Fmt.stext%" "%Fmt.shown%")")
              uName uOld uNew uStatus
       
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
  

data Results = Results { rKept :: Set PackageWithPath
                       , rDeclared :: Map PackageWithPath PkgStatus
                       , rInstalled :: Map PackageWithPath PkgStatus }
  deriving Show

removing,installing,wantedFromDeclared :: Results -> Map PackageWithPath PkgStatus
removing r@Results{ rInstalled, rDeclared } = 
  removeKeys (rInstalled M.\\ rDeclared) (wantedFromKept r)
installing r@Results{ rInstalled } = 
  (wantedFromDeclared r M.\\ rInstalled) 
wantedFromDeclared r@Results{  rDeclared } = 
  removeKeys rDeclared (S.map newPackage (updatesFromKept r))

wantedFromKept :: Results -> Set PackageWithPath
wantedFromKept r = S.map (oldPackage) (updatesFromKept r)

updatesFromKept:: Results -> Set Upd
updatesFromKept Results{ rKept, rDeclared }  = us
  where (us, _, _) = calculateUpdates rDeclared rKept

blockedUpdates :: Results -> Set Upd
blockedUpdates r@Results{ rInstalled } = S.filter isNonTrivial us
  where (us, _,_) = calculateUpdates 
                      (M.fromList . map newPackageAndStatus . S.toList $ updatesFromKept r)
                      (M.keysSet rInstalled)

keptUpdates :: Results -> Set Upd
keptUpdates Results{ rInstalled, rKept } = S.filter isNonTrivial us
  where (us, _,_) = calculateUpdates (addStorePathStatus rKept) (M.keysSet rInstalled)
        -- TODO: actually rKept could have a proper status from the start
        addStorePathStatus = M.fromSet (const Present) 

isStrictUpdate, isReinstall, isNonTrivial :: Upd -> Bool
isStrictUpdate Upd{uOld, uNew} = uOld /= uNew
isReinstall Upd{..} = uOld == uNew && uOldPath /= uNewPath
isNonTrivial u = isStrictUpdate u || isReinstall u
