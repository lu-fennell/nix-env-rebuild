{-# LANGUAGE NamedFieldPuns, NoImplicitPrelude, OverloadedStrings, ExtendedDefaultRules, RecordWildCards, NoMonomorphismRestriction, TemplateHaskell, QuasiQuotes, RecordWildCards #-}

module Nix.OutputParser where 

import BasicPrelude hiding (try, (</>), (<.>), FilePath, rem, takeWhile)
import Control.Applicative.QQ.ADo
import Data.Monoid
import Control.Lens
import Data.Text.Lens
import qualified Data.Char as Char
import qualified Data.Text as Text
import Data.Attoparsec.Text
import Shelly

import Nix.Types 

parseNixOutput :: Parser (Maybe (Package, StorePath, PkgStatus)) 
               -> Sh Text 
               -> Sh [PackageWithPath]
parseNixOutput p c = 
  filterErrors . map (\l -> addErrorTitle l . parsePackageWithPath p $ l) . lines 
  =<< c
  where filterErrors ps = do
          forM_ (lefts ps) $ echo_err
          return $ catMaybes $ rights ps

        addErrorTitle l = over _Left (\e -> "** Do not understand `" <> l <> "': " <> showT e)

        showT = show
                         

parsePackageWithPath :: Parser (Maybe (Package, StorePath, PkgStatus)) 
                     -> Text 
                     -> Either String (Maybe PackageWithPath)
parsePackageWithPath parser = over (_Right._Just) 
                         (\(n, p, s) -> Pwp { pwpPkg = parseVersionedPackage n 
                                            , pwpPath = p
                                            , pwpStatus = s
                                            })
                         . parseOnly parser
       
fromInstallAction :: Parser Text -> Parser Package
fromInstallAction prefix = skipSpace *> prefix *> skipSpace 
                             *> ("`" *> takeTill (=='\'')) 

fromInstalling, fromUninstalling :: Parser Package
fromInstalling =  fromInstallAction $ string "installing"
fromUninstalling =  fromInstallAction $ string "uninstalling"
                   

fromFileList :: Parser Text -> Parser Package
fromFileList packageName =
  skipSpace *>
  "/nix/store" 
  *> takeTill (== '-') *> skip (const True)
  *> packageName 

fromBuilding :: Parser Package
fromBuilding =
  fromFileList (view packed <$> manyTill anyChar ".drv")

fromFetching :: Parser Package 
fromFetching = (fromFileList takeText)
               
fromLocalQuery, fromRemoteQuery :: Parser (Maybe (Package, StorePath, PkgStatus))
fromLocalQuery = over (mapped._Just) 
                        (\(pkg, path) -> (pkg, path, Present)) 
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

wouldInstall, wouldRemove :: Parser (Maybe Package)
wouldInstall =  (Just <$> fromBuilding)
               <|> (Just <$> fromInstalling)
               <|> (Nothing <$ fromFetching) -- for now, ignore fetched packages
wouldRemove = Just <$> fromUninstalling
