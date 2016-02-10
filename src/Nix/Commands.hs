{-# LANGUAGE NamedFieldPuns, NoImplicitPrelude, OverloadedStrings, ExtendedDefaultRules, RecordWildCards, NoMonomorphismRestriction, TemplateHaskell, QuasiQuotes, RecordWildCards #-}

module Nix.Commands where

import BasicPrelude hiding (try, (</>), (<.>), FilePath, rem, takeWhile)
import Shelly hiding (path)
import qualified Data.Text as Text

import Utils

data Config = Config { cfgDeclaredPackages :: FilePath
                     , cfgDeclaredOutPaths :: FilePath
                     , cfgKeepAroundProfile :: FilePath
                     , cfgDestProfile :: FilePath
     }
  deriving (Show)
       
data Nix = Nix { nixCommand :: NixCmd
               , nixSelection :: Maybe [Text] -- package names given on the cmdline
               , nixDryRun :: Bool
               , nixProfile :: Maybe FilePath
               , nixFile :: Maybe FilePath
               }
  deriving (Show)

data NixCmd = NixInstall NixInstallOptions
            | NixUninstall
            | NixQueryLocal 
            | NixQueryRemote
  deriving (Show)
  
isQuery :: NixCmd -> Bool
isQuery NixQueryLocal = True
isQuery NixQueryRemote = True
isQuery _ = False

data NixInstallOptions = NIOs { nioRemoveAll :: Bool
                              , nioFromProfile :: Maybe FilePath
                              }
  deriving Show

nixCmdStrings :: Nix -> (FilePath, [Text])
nixCmdStrings c@Nix{..} = ("nix-env", dryRun ++ profile ++ file ++ nixcmd ++ selection)
  where dryRun = if nixDryRun then ["--dry-run"] else []
        profile = Utils.maybeOpt "--profile" nixProfile
        selection = fromMaybe ["*"] $ map assertNonEmpty nixSelection
        file = Utils.maybeOpt "--file" nixFile
        nixcmd = nixCmdArgs nixCommand
        assertNonEmpty [] = error $ "nixCmdStrings: empty selection list\n" ++ (Text.unpack . show $ c)
        assertNonEmpty xs = xs



nixCmdArgs :: NixCmd -> [Text]
nixCmdArgs (NixInstall NIOs{..}) = 
  ["--install"]
  ++ concat
  [ guard nioRemoveAll >> return "--remove-all"
  , Utils.maybeOpt "--from-profile" nioFromProfile
  ]
nixCmdArgs NixUninstall = ["-e"]
nixCmdArgs NixQueryLocal = ["-q", "--out-path"]
nixCmdArgs NixQueryRemote = ["-qa", "--out-path", "--status"]

        
-- | Default, dry-run version of a nix command.
nixDefault :: NixCmd -> Nix
nixDefault nixcmd = Nix nixcmd Nothing True Nothing Nothing
           
nixDestProfile :: Config -> NixCmd -> Nix
nixDestProfile (Config{cfgDestProfile}) nixcmd = (nixDefault nixcmd) { nixProfile = Just cfgDestProfile }
               
installPackages, installKeep :: Config -> [Text] -> Nix
installPackages cfg@Config{..} ps =  (nixDestProfile cfg (NixInstall $ NIOs False Nothing))
                          { nixFile = Just cfgDeclaredPackages
                          , nixSelection = Just ps
                          } 
installKeep cfg@Config{..} ps = (nixDestProfile cfg (NixInstall $ NIOs False $ Just cfgKeepAroundProfile))
                     { nixSelection = Just ps
                     } 

removePackages, switchToNewPackages :: Config -> Nix
removePackages cfg@Config{..} = (nixDestProfile cfg NixUninstall)
switchToNewPackages Config{..} = nixDefault (NixInstall $ NIOs True (Just cfgDestProfile)) 
                                       
nixCmd_ :: Nix -> Sh ()
nixCmd_ n = uncurry run_ $ nixCmdStrings n
nixCmd, nixCmdErr :: Nix -> Sh Text
nixCmd n = uncurry run $ nixCmdStrings n
nixCmdErr n = Utils.runStderr nix args
  where (nix, args) = nixCmdStrings n


  
