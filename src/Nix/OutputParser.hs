{-# LANGUAGE NamedFieldPuns, NoImplicitPrelude, OverloadedStrings, ExtendedDefaultRules, RecordWildCards, NoMonomorphismRestriction, TemplateHaskell, QuasiQuotes, RecordWildCards #-}

module Nix.OutputParser where 

import BasicPrelude hiding (try, (</>), (<.>), FilePath, rem, takeWhile)
import Control.Applicative.QQ.ADo
import Control.Lens
import Data.Text.Lens
import qualified Data.Char as Char
import qualified Data.Text as Text
import Data.Attoparsec.Text
import Shelly

import Nix.Types 

parseNixOutput :: Parser (Maybe (Package, StorePath, PkgStatus)) 
               -> Sh Text 
               -> Sh [(PackageWithPath, PkgStatus)]
parseNixOutput p c = 
  filterErrors . map (\l -> addErrorTitle l . parsePackageWithPath p $ l) . lines 
  =<< c
  where filterErrors ps = do
          forM_ (lefts ps) $ echo_err
          return $ catMaybes $ rights ps

        addErrorTitle l = over _Left (\e -> "** Do not understand `" <> l <> "': " <> showT e)

        showT = show
                         

-- | run a package returning a (package, store-path, status) tripple
-- TODO: do not require to return a status
parsePackageWithPath :: Parser (Maybe (Package, StorePath, PkgStatus)) 
                     -> Text  -- ^ input to parse
                     -> Either String (Maybe (PackageWithPath, PkgStatus))
parsePackageWithPath parser = over (_Right._Just) mkPackageWithPath . parseOnly parser
  where mkPackageWithPath (n, p, s) = (Pwp { pwpPkg = parseVersionedPackage n 
                                       , pwpPath = p}
                                       , s)
       

fromInstalling, fromUninstalling :: Parser Package
-- | parse a package from store path prefixed with "installing". 
fromInstalling =  fromQuoted $ string "installing"
-- | parse a package from store path prefixed with "uninstalling". 
fromUninstalling =  fromQuoted $ string "uninstalling"
                   
-- | parses a package name and version between quotes with given
-- prefix (e.g. "installing `subversion-1.7.13'")
fromQuoted :: Parser Text -> Parser Package
fromQuoted prefix = skipSpace *> prefix *> skipSpace *> ("`" *> takeTill (=='\'')) 

fromStorePath :: FilePath -> Parser Text -> Parser Package 
fromStorePath basedir packageName = 
     skipSpace 
  *> string (toTextIgnore basedir)
  *> takeTill (== '-') *> skip (const True)
  *> packageName 

fromBuilding :: FilePath -> Parser Package
fromBuilding basedir =
  fromStorePath basedir (view packed <$> manyTill anyChar ".drv")

fromFetching :: FilePath -> Parser Package 
fromFetching basedir = (fromStorePath basedir takeText)
               
fromLocalQuery, fromRemoteQuery :: Parser (Maybe (Package, StorePath, PkgStatus))
fromLocalQuery = over (mapped._Just) 
                        (\(pkg, p) -> (pkg, p, Present)) 
                        fromQuery
fromRemoteQuery = do
  status <- status
  skipSpace
  mq <- fromQuery
  return $ over _Just (\(pkg, path) -> (pkg, path, status)) mq
  
status :: Parser PkgStatus
status = [ado|
             prebuiltCode <- code *> code *> code
             if prebuiltCode == 'S' then Prebuilt else Source
           |]
 where code = satisfy (inClass "-A-Z")

fromQuery :: Parser (Maybe (Package, StorePath))
fromQuery = do 
                name <- takeWhile (not . Char.isSpace)
                skipSpace
                path <- takeText
                guard (not . Text.null $ path)
                guard (Text.all (not . Char.isSpace) path)
                return $ Just (name, path)

wouldInstall:: FilePath -> Parser (Maybe Package)
wouldInstall basedir =  (Just <$> fromBuilding basedir)
               <|> (Just <$> fromInstalling)
               <|> (Nothing <$ fromFetching basedir) -- for now, ignore fetched packages
wouldRemove :: Parser (Maybe Package)
wouldRemove = Just <$> fromUninstalling
