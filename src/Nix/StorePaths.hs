{-# LANGUAGE NamedFieldPuns, NoImplicitPrelude, OverloadedStrings, ExtendedDefaultRules, RecordWildCards, NoMonomorphismRestriction, TemplateHaskell, QuasiQuotes, RecordWildCards #-}

module Nix.StorePaths where 

import BasicPrelude hiding (try, (</>), (<.>), FilePath, rem, takeWhile)
import Control.Lens
import Data.Text.Lens
import qualified Data.Text as T
import qualified Data.Attoparsec.Text as Attoparsec
import Text.Printf.TH (st)

import Nix.Packages       
import qualified Nix.OutputParser as P


storePathLines :: Text -> [Text]
storePathLines = (filter (\l -> not ("#" `T.isPrefixOf` l) && not (T.null l)) . map T.strip . T.lines)

parsePackageFromStorePath :: Text -> Either Text PackageWithPath
parsePackageFromStorePath t = over _Left mkErrorMsg
                            . fmap (addStoreDir t . parseVersionedPackage)
                            . Attoparsec.parseOnly (P.fromStorePath "" Attoparsec.takeText) $ t
  where mkErrorMsg msg = [st|error parsing store path %s: %s|] t (T.pack msg)
        addStoreDir path p = Pwp { pwpPkg = p , pwpPath = path}


parsePackageFromStorePathContents :: Text -> [Either Text PackageWithPath]
parsePackageFromStorePathContents = map parsePackageFromStorePath . storePathLines
                              
