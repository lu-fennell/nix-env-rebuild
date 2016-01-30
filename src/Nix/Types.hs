{-# LANGUAGE NamedFieldPuns, NoImplicitPrelude, OverloadedStrings, ExtendedDefaultRules, RecordWildCards, NoMonomorphismRestriction, TemplateHaskell, QuasiQuotes, RecordWildCards #-}

module Nix.Types where 

import BasicPrelude hiding (try, (</>), (<.>), FilePath, show, rem, takeWhile)

import qualified Data.Text as T
import qualified Nix.Utils as Utils
import Formatting ((%))
import qualified Formatting as Fmt
import qualified Data.Set as S

-- -------------------------------------------------------------------
-- Packages
-- -------------------------------------------------------------------

type PackageName = Text
type Package = Text
type StorePath = Text
data VersionedPackage = VPkg { pName :: PackageName
                             , pVer :: Utils.PackageVersion 
                             }
  deriving (Eq, Ord, Show)
  
data PackageWithPath = Pwp { pwpPkg :: VersionedPackage
                           , pwpStatus :: PkgStatus
                           , pwpPath :: StorePath
                           }
  deriving (Eq, Ord, Show)
  
data PkgStatus = Present | Prebuilt | Source
  deriving (Eq, Ord, Show)
           
parseVersionedPackage :: Package -> VersionedPackage
parseVersionedPackage p = VPkg{..}
  where (pName, pVer) = Utils.splitPackage p

  
formatVersionPackage :: VersionedPackage -> PackageName
formatVersionPackage (VPkg {..}) = if T.null pVer 
                                   then pName 
                                   else pName <> "-" <> pVer

formatPackageWithPath :: PackageWithPath -> PackageName
formatPackageWithPath = formatVersionPackage . pwpPkg



-- -------------------------------------------------------------------
-- Updates
-- -------------------------------------------------------------------
data Upd = Upd { uName :: Package
               , uOld :: Utils.PackageVersion 
               , uOldPath :: StorePath
               , uNew :: Utils.PackageVersion 
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
                         , pwpPath = uOldPath 
                         , pwpStatus = Present }
newPackage Upd{..} = Pwp { pwpPkg = VPkg{pName = uName, pVer = uNew}
                         , pwpPath = uNewPath 
                         , pwpStatus = uStatus 
                         }

filterUpds :: Set PackageWithPath -- ^ added packages
           -> Set PackageWithPath -- ^ removed packages
           -> (Set Upd, Set PackageWithPath, Set PackageWithPath)
filterUpds fresh rem = (upds, fresh', removing')
    where upds = S.fromList $
                 [upd | fr <- S.toList fresh 
                      , re <- S.toList rem
                      , upd <- maybeToList $ findUpdate fr re
                 ]
          removing' = rem S.\\ S.map oldPackage upds 
          fresh' = fresh S.\\ S.map newPackage upds

-- | Determine if an added package and a removed package form an update.
findUpdate :: PackageWithPath -- ^ Added package
           -> PackageWithPath -- ^ Removed package
           -> Maybe Upd
findUpdate ad re = mkUpd ad re
  where mkUpd (Pwp { pwpPkg = (VPkg { pName = uName , pVer = uNew})
                   , pwpPath = uNewPath
                   , pwpStatus = uStatus
                   }) 
              (Pwp { pwpPkg = (VPkg { pName = n2, pVer =  uOld})
                   , pwpPath = uOldPath
                   }) 
                | uName == n2  = Just (Upd {..})
                | otherwise = Nothing
  

data Results = Results { rKept :: Set PackageWithPath
                       , rDeclared :: Set PackageWithPath
                       , rInstalled :: Set PackageWithPath }
  deriving Show

removing,installing,wantedFromDeclared, wantedFromKept :: Results -> Set PackageWithPath
updatesFromKept, blockedUpdates, keptUpdates :: Results -> Set Upd
removing r@Results{ rInstalled, rDeclared } = 
  (rInstalled S.\\ rDeclared) S.\\ wantedFromKept r
installing r@Results{ rInstalled } = 
  (wantedFromDeclared r S.\\ rInstalled) 
wantedFromDeclared r@Results{  rDeclared } = 
  rDeclared S.\\ S.map newPackage (updatesFromKept r)
wantedFromKept r = S.map (oldPackage) (updatesFromKept r)

updatesFromKept Results{ rKept, rDeclared }  = us
  where (us, _, _) = filterUpds rDeclared rKept
  
blockedUpdates r@Results{ rInstalled } = S.filter isNonTrivial us
  where (us, _,_) = filterUpds (S.map newPackage (updatesFromKept r))
                               rInstalled
  
keptUpdates Results{ rInstalled, rKept } = S.filter isNonTrivial us
  where (us, _,_) = filterUpds rKept rInstalled

isStrictUpdate, isReinstall, isNonTrivial :: Upd -> Bool
isStrictUpdate Upd{uOld, uNew} = uOld /= uNew
isReinstall Upd{..} = uOld == uNew && uOldPath /= uNewPath
isNonTrivial u = isStrictUpdate u || isReinstall u
