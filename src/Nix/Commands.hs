{-# LANGUAGE NamedFieldPuns, NoImplicitPrelude, OverloadedStrings, ExtendedDefaultRules, RecordWildCards, NoMonomorphismRestriction, TemplateHaskell, QuasiQuotes, RecordWildCards #-}

module Nix.Commands where

import BasicPrelude hiding (try, (</>), (<.>), FilePath, rem, takeWhile)
import Shelly hiding (path)
import qualified Data.Text as Text
import Control.Lens

import Utils

data Config = Config { cfgProfile :: FilePath -- ^ Path to the nix profile 
                                                    --  (if default should not be used)
                     , cfgDeclaredPackages :: FilePath -- ^ Path to the declared package expressions
                     , cfgDeclaredOutPaths :: FilePath -- ^ Path the the list of store path to be installed
                     -- TODO: rename DestProfile to CacheProfile
                     , cfgDestProfile :: FilePath -- ^ Path to the cache profile
                     , cfgInclude :: Maybe Text
     }
  deriving (Show)
       
data Nix = Nix { nixCommand :: NixCmd
               , nixSelection :: Maybe [Text] -- package names given on the cmdline
               , nixDryRun :: Bool
               , nixProfile :: FilePath
               , nixFile :: Maybe FilePath
               , nixInclude :: Maybe Text
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
nixCmdStrings c@Nix{..} = ("nix-env", dryRun ++ profile ++ file ++ includes ++ nixcmd ++ selection)
  where dryRun = if nixDryRun then ["--dry-run"] else []
        profile = ["--profile", nixProfile^.fpText]
        selection = fromMaybe ["*"] $ map assertNonEmpty nixSelection
        file = Utils.maybeOpt "--file" (fmap toTextIgnore nixFile)
        includes = Utils.maybeOpt "-I" nixInclude
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
  , Utils.maybeOpt "--from-profile" (fmap toTextIgnore nioFromProfile)
  ]
nixCmdArgs NixUninstall = ["-e"]
nixCmdArgs NixQueryLocal = ["-q", "--out-path"]
nixCmdArgs NixQueryRemote = ["-qa", "--out-path", "--status"]

        
-- | Default, dry-run version of a nix command.
nixDefault :: FilePath -> Maybe Text -> NixCmd -> Nix
nixDefault profile includes nixcmd = Nix { nixCommand = nixcmd 
                        , nixSelection = Nothing 
                        , nixDryRun = True 
                        , nixProfile = profile
                        , nixFile = Nothing
                        , nixInclude = includes
                        }
           
-- | perform a NixCmd for the destination profile
nixDestProfile :: Config -> NixCmd -> Nix
nixDestProfile (Config{..}) nixcmd = nixDefault cfgDestProfile cfgInclude nixcmd 

-- | install selected packages to the destination profile
installPackages :: Maybe FilePath -> Config -> [Text] -> Nix
installPackages fromProfile cfg@Config{..} ps =  
   (nixDestProfile cfg (NixInstall $ NIOs {nioRemoveAll = False, nioFromProfile = fromProfile}))
                          { nixFile = Just cfgDeclaredPackages
                          , nixSelection = Just ps
                          } 

-- | install a list of store paths to the destination profile
installStorePaths :: Config -> [Text] -> Nix
installStorePaths cfg paths = 
  (nixDestProfile cfg (NixInstall (NIOs{ nioRemoveAll = False, nioFromProfile = Nothing })))
      { nixSelection = Just paths
      , nixDryRun = False
      }

removePackages, switchToNewPackages :: Config -> Nix
removePackages cfg@Config{..} = (nixDestProfile cfg NixUninstall)
switchToNewPackages Config{..} = 
  (nixDefault cfgProfile cfgInclude 
              (NixInstall $ NIOs { nioRemoveAll = True
                                 , nioFromProfile = Just cfgDestProfile}))
                                       
-- | Run a Nix command, ignoring output
nixCmd_ :: Nix -> Sh ()
nixCmd_ n = uncurry run_ $ nixCmdStrings n

-- | Run a Nix command, storing output as Text
nixCmd, nixCmdErr :: Nix -> Sh Text
nixCmd n = uncurry run $ nixCmdStrings n
nixCmdErr n = Utils.runStderr nix args
  where (nix, args) = nixCmdStrings n
