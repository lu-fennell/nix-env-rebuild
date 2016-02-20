{-# LANGUAGE NamedFieldPuns, NoImplicitPrelude, OverloadedStrings, ExtendedDefaultRules, RecordWildCards, NoMonomorphismRestriction, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
    
module Main where

import BasicPrelude hiding (try, (</>), (<.>), FilePath, show, rem, takeWhile)
import Control.Applicative.QQ.ADo
import qualified Data.Text as T
import Options.Applicative hiding (Parser)
import qualified Options.Applicative as Opt
import Shelly hiding (path)
import Text.PrettyPrint (($$))
import qualified Text.PrettyPrint as PP
import Data.Attoparsec.Text as Attoparsec
import Control.Lens
import Text.Printf.TH (st)
  

import qualified Data.Set as S
import qualified Data.Map as M

import Utils ((<$/!>))
import qualified Utils
import qualified Nix.OutputParser as P
import Nix.Types
import Nix.Commands

default (Text)


---------------------------
-- Configuration and command line parsing
---------------------------
data Opt = Opt { optCfg :: Config
               , optCommand :: Command
               , optVerbose :: Bool
               }
  deriving (Show)

  
data Command = DryRun | Build | Switch
  deriving (Show, Eq)

  
-- default locations
readDeclaredPackages, readDestProfile, readOutPathList, readProfile :: Sh FilePath
readDeclaredPackages = "HOME" <$/!> ".nixpkgs/packages.nix" 
readDestProfile = "NIX_USER_PROFILE_DIR" <$/!> "nix-rebuild-cache"
readOutPathList = "HOME" <$/!> ".nixpkgs/store-path-install-list.txt"
readProfile = "HOME" <$/!> ".nix-profile"
                      
tmpProfileName :: Text
tmpProfileName = "tmp-nix-rebuild-profile"
               
getConfig :: Sh (Opt)
getConfig = do
   declaredPackages <- readDeclaredPackages 
   outPathList <- readOutPathList
   destProfile <- readDestProfile
   profile <- readProfile
   let flags :: Opt.Parser (Opt)
       flags = [ado|
                   cfgProfile <- Opt.option Utils.fileReader
                     (long "profile"
                     <> short 'p'
                     <> metavar "DIR"
                     <> value profile <> showDefault
                     <> help "Target profile to install packages to"
                     ) 
                   cfgDeclaredPackages <- Opt.option Utils.fileReader 
                     (long "packages"
                      <> metavar "FILE"
                      <> value declaredPackages <> showDefault
                      <> help "File containing the list of packages to be installed. \
                              \It should contain a nix-expression evaluating to a list of derivations \ 
                              \(as for: nix-env -i -f FILE)")
                   cfgDeclaredOutPaths <- Opt.option Utils.fileReader
                     (long "out-path-list"
                     <> value outPathList <> showDefault
                     <> help "File containing the list of store-path to be installed, \
                             \one store path per line."
                     )
                   cfgDestProfile <- Opt.option Utils.fileReader
                     (long "cache-profile"
                     <> metavar "DIR"
                     <> value destProfile <> showDefault
                     <> help "Profile to store the build result into"
                     )
                   optVerbose <- flag False True 
                     (long "verbose" <> short 'v'
                     <> help "echo nix commands and their output") 
                   optCommand <- subparser 
                     (  (Opt.command "dry-run" $ info (pure DryRun) 
                          (progDesc "Only show what would change"))
                     <> (Opt.command "build" $ info (pure Build) 
                          (progDesc "Build into cache profile but do not switch the target profile"))
                     <> (Opt.command "switch" $ info (pure Switch) 
                          (progDesc "Build packages into cache profile and switch the target profile")))
                     <|>
                     pure DryRun
                   
                   Opt {  optCfg = Config { 
                              cfgProfile = cfgProfile
                            , cfgDeclaredPackages = cfgDeclaredPackages
                            , cfgDeclaredOutPaths = cfgDeclaredOutPaths
                            , cfgDestProfile = cfgDestProfile 
                          }
                          , optCommand = optCommand
                          , optVerbose = optVerbose 
                          } 
                   |]
   liftIO $ execParser
     $ info (helper <*> flags)
            (fullDesc <> progDesc "Declaratively manage the nix user environment")
                                  
        
---------------------------
-- Main
---------------------------
main :: IO ()
main = shelly $ silently $ do
  opt@Opt{..} <- getConfig
  (if optVerbose then verbosely else silently) $ do
    r <- getResults optCfg
    report optCfg r
    when (optCommand /= DryRun) $ do
      (if optVerbose then withOutput else id) $ doInstall opt r
    echo $ "\n" <> finishMessage opt

  where finishMessage Opt{..} = case optCommand of
          DryRun -> "Dry run. Not doing anyting."
          Build -> "Rebuild completed in profile "<> toTextIgnore (cfgDestProfile optCfg)
          Switch -> "Rebuild done"

        report cfg r = echo $ T.pack (PP.render (makeReport cfg r))

        withOutput = print_stdout True . print_stderr True

getResults :: Config -> Sh Results
getResults cfg@Config{..} = do
           rStorePaths <- getStorePaths cfg
           rDeclared <- fmap (M.fromList . S.toList) $ parseNix P.fromRemoteQuery
                             (nixCmd $ (nixDefault cfgDestProfile NixQueryRemote)
                              {nixFile = Just cfgDeclaredPackages})
           rInstalled <- fmap (M.fromList . S.toList) $ parseNix P.fromLocalQuery
                             (nixCmd (nixDefault cfgProfile NixQueryLocal))

           return Results{..}
        where parseNix p c = S.fromList <$> P.parseNixOutput p c

-- TODO: should probably move to another module (Command.hs?)
getStorePaths :: Config -> Sh (Set PackageWithPath)
getStorePaths Config{..} = do
  fileExists <- test_f cfgDeclaredOutPaths
  if fileExists 
   then do
     paths <- fmap (map T.strip . T.lines) . readfile $ cfgDeclaredOutPaths
     fmap S.fromList . mapM (\p -> addStoreDir p =<< (parsePackageFromPath p)) $ paths
   else do
    echo_err $ [st|Warning: installed store paths file does not exist (%s)|] 
               (cfgDeclaredOutPaths^.Utils.fpText)
    return (S.empty)
  where parsePackageFromPath = Utils.fromJustThrow 
                               . fmap parseVersionedPackage 
                               . Attoparsec.parseOnly (P.fromStorePath "" takeText) 
                                                      -- out paths are given relative to the store dir 
                                                      -- TODO: move this parser in the OutputParser module
        addStoreDir path p = return $ Pwp { pwpPkg = p
                                          , pwpPath = path 
                                          }

makeReport :: Config -> Results -> PP.Doc
makeReport Config{..} r = 
  let (upds, install', removing') = 
        calculateUpdates (installing r) (M.keysSet . removing $ r)
      versionUpdates = S.filter isStrictUpdate upds
      reinstalls = S.map newPackageAndStatus $ upds S.\\ versionUpdates
      sourceReinstalls = S.filter (\(_, pwpStatus) -> pwpStatus == Source) 
                                  reinstalls
  in PP.text "Updating:" $$ PP.empty
     $$ PP.nest 2 (formatSet formatUpd versionUpdates)
     $$ PP.char ' ' $$
     PP.text "Adding:" $$ PP.empty
     $$ PP.nest 2 (formatSet formatPackageWithPath . M.keysSet $ install')
     $$ PP.char ' ' $$
     PP.text "Reinstalling from source:" $$ PP.empty
     $$ PP.nest 2 (formatSet formatPackageWithPath . S.map fst $ sourceReinstalls)
     $$ PP.char ' ' $$
     PP.text "Removing:" 
     $$ PP.nest 2 (formatSet formatPackageWithPath removing')
     $$ PP.char ' ' $$
     pptext "Updates through store-path packages:"
     $$ PP.nest 2 (formatSet formatUpd (updatingStorePaths r))
     $$ PP.char ' ' $$
     pptext "Updates and reinstalls blocked by store-path packages:"
     $$ PP.nest 2 (formatSet formatUpd (blockedUpdates r))
  
  where formatSet f s | S.null s  = PP.text "<none>"
                      | otherwise = PP.vcat . map (pptext . f) 
                                    . S.toList $ s
        pptext = PP.text . T.unpack

  
---------------------------
-- Running nix-env
---------------------------
doInstall :: Opt -> Results -> Sh ()
doInstall Opt{..} r = do
  echo $ [st|\n* Installing packages from %s to cache profile \n   (%s)|] 
         (toTextIgnore . cfgDeclaredPackages $ optCfg)
         (toTextIgnore . cfgDestProfile $ optCfg)
  nixCmdCfgExecute $ removePackages optCfg
  pkgCmd "install package list" installPackages 
    . map formatPackageWithPath 
    . S.toList . M.keysSet 
    . wantedFromDeclared $ r
  echo $ [st|\n* Installing packages from %s to cache profile \n   (%s)|] 
         (toTextIgnore . cfgDeclaredOutPaths $ optCfg)
         (toTextIgnore . cfgDestProfile $ optCfg)
  pkgCmd "install store paths" installStorePaths (map pwpPath . S.toList . rStorePaths $ r)
  when (optCommand == Switch) $ do
    echo $ [st|\n* Switching profile %s|] (toTextIgnore . cfgProfile $ optCfg)
    nixCmdCfgExecute $ switchToNewPackages optCfg
  where pkgCmd source installCmd ps = 
          if null ps 
          then echo_err $ "nix-rebuild: Nothing to be installed from " <> source
          else nixCmdCfgExecute $ installCmd optCfg ps
        nixCmdCfgExecute = nixCmd_ . \n -> n { nixDryRun = optCommand == DryRun }
