{-# LANGUAGE NamedFieldPuns, NoImplicitPrelude, OverloadedStrings, ExtendedDefaultRules, RecordWildCards, NoMonomorphismRestriction, TemplateHaskell, QuasiQuotes, RecordWildCards #-}

module Nix.Commands where

import BasicPrelude hiding (try, (</>), (<.>), FilePath, rem, takeWhile)
import Shelly hiding (path)
import qualified Data.Text as Text
import Control.Lens

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



-- -------------------------------------------------------------------
-- Construct the nix command line
-- -------------------------------------------------------------------
-- | Construct a basic command line calling according to NixCmd
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
               
-- | install selected packages to the destination profile
installPackages :: Config -> [Text] -> Nix
installPackages cfg@Config{..} ps =  (nixDestProfile cfg (NixInstall $ NIOs False Nothing))
                          { nixFile = Just cfgDeclaredPackages
                          , nixSelection = Just ps
                          } 

-- | install the entire keep-around profile to the destination
installKeep :: Config -> [Text] -> Nix
installKeep cfg@Config{..} ps = 
  (nixDestProfile cfg (NixInstall $ NIOs { nioRemoveAll = False
                                         , nioFromProfile = Just cfgKeepAroundProfile}))
     {nixSelection = Just ps} 

-- | install a list of store paths to the keep-around profile
installToKeep :: Config -> [Text] -> Nix
installToKeep Config{..} paths = 
  (nixDefault (NixInstall (NIOs{ nioRemoveAll = True, nioFromProfile = Nothing })))
      { nixSelection = Just paths
      , nixDryRun = False
      , nixProfile = Just cfgKeepAroundProfile
      }

removePackages, switchToNewPackages :: Config -> Nix
removePackages cfg@Config{..} = (nixDestProfile cfg NixUninstall)
switchToNewPackages Config{..} = nixDefault (NixInstall $ NIOs True (Just cfgDestProfile)) 
                                       
-- | Run a Nix command, ignoring output
nixCmd_ :: Nix -> Sh ()
nixCmd_ n = uncurry run_ $ nixCmdStrings n

-- | Run a Nix command, storing output as Text
nixCmd, nixCmdErr :: Nix -> Sh Text
nixCmd n = uncurry run $ nixCmdStrings n
nixCmdErr n = Utils.runStderr nix args
  where (nix, args) = nixCmdStrings n


  
