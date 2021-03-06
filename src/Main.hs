{-# LANGUAGE NamedFieldPuns, NoImplicitPrelude, OverloadedStrings, ExtendedDefaultRules, RecordWildCards, NoMonomorphismRestriction, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
    
module Main where

import BasicPrelude hiding (try, (</>), (<.>), FilePath, show, rem, takeWhile)
import qualified Prelude
import Control.Applicative.QQ.ADo
import qualified Data.Text as T
import qualified Data.Text.IO as T
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
import Nix.Packages
import Nix.Commands
import Nix.StorePaths

default (Text)


---------------------------
-- Configuration and command line parsing
---------------------------
data Opt = Opt { optCfg :: Config
               , optCommand :: Command
               , optVerbose :: Bool
               , optKeepInstalled :: Bool
               }
  deriving (Show)

  
data Command = DryRun | Build | Switch
  deriving (Show, Eq)

  
-- default locations
readDeclaredPackages, readDestProfile, readOutPathList, readProfile :: Sh FilePath
readDeclaredPackages = "HOME" <$/!> ".nixpkgs/packages.nix" 
readDestProfile = "NIX_USER_PROFILE_DIR" <$/!> "nix-env-rebuild-cache"
readOutPathList = "HOME" <$/!> ".nixpkgs/store-paths.txt"
readProfile = Utils.readSymbolicLink =<< ("HOME" <$/!> ".nix-profile")
                      
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
                     (long "store-path-list"
                     <> value outPathList <> showDefault
                     <> help "File containing the list of store-paths to be installed, \
                             \one store path per line."
                     )
                   cfgDestProfile <- Opt.option Utils.fileReader
                     (long "cache-profile"
                     <> metavar "DIR"
                     <> value destProfile <> showDefault
                     <> help "Profile to store the build result into"
                     )
                   cfgInclude <- Opt.option (fmap Just Utils.textReader)
                     (short 'I'
                     <> Opt.metavar "PATH"
                     <> help "Add a path to the Nix expression search path")
                     <|> pure Nothing
                   optKeepInstalled <- flag False True
                     (long "keep-installed"
                     <> help "do not remove undeclared packages currently installed"
                     )
                   optVerbose <- flag True False
                     (long "quiet" <> short 'q'
                     <> help "suppress output of nix commands"
                     )
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
                            , cfgInclude = cfgInclude
                          }
                          , optCommand = optCommand
                          , optVerbose = optVerbose 
                          , optKeepInstalled = optKeepInstalled
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
  checkConfig optCfg
  searchPath <- get_env_text "NIX_PATH"
  silently $ do
    echo $ "* Calculating updates"
    echo $ [st|  - packages from %s|] (toTextIgnore . cfgDeclaredPackages $ optCfg)
    echo $ [st|  - store-paths from %s|] (toTextIgnore . cfgDeclaredOutPaths $ optCfg)
    echo $ [st|  - search path %s|] (maybe searchPath (\p -> [st|%s:%s|] p searchPath) (cfgInclude optCfg))
    echo ""
    r <- getResults optCfg
    report optKeepInstalled optCfg r
    when (optCommand /= DryRun) $ do
      (if optVerbose then withOutput else id) $ doInstall opt r
    echo $ "\n" <> finishMessage opt

  where finishMessage Opt{..} = case optCommand of
          DryRun -> "Dry run. Not doing anything."
          Build -> "Rebuild completed in profile "<> toTextIgnore (cfgDestProfile optCfg)
          Switch -> "Rebuild done"

        report keepInstalled cfg r = echo $ T.pack (PP.render (makeReport keepInstalled cfg r))

        withOutput = print_stdout True . print_stderr True

checkConfig :: Config -> Sh ()
checkConfig Config{..} = do
  unlessM (test_f cfgDeclaredPackages) $ do
    errorExit $ [st|Package file `%s' does not exist. Aborting.|] (toTextIgnore cfgDeclaredPackages)
  -- the existance of the store-path list is not mandaroty.. but a
  -- missing list will emit a warming later on.

getResults :: Config -> Sh Results
getResults cfg@Config{..} = do
           rStorePaths <- getStorePaths cfg
           -- TODO: this does not return the real declared packages if NIX_PATH is wrong
           rDeclared <- fmap (M.fromList . S.toList) $ parseNix P.fromRemoteQuery
                             (nixCmd $ (nixDefault cfgDestProfile cfgInclude NixQueryRemote)
                              {nixFile = Just cfgDeclaredPackages})
           rInstalled <- fmap (M.fromList . S.toList) $ parseNix P.fromLocalQuery
                             (nixCmd (nixDefault cfgProfile cfgInclude NixQueryLocal))

           return Results{..}
        where parseNix p c = S.fromList <$> P.parseNixOutput p c

getStorePaths :: Config -> Sh (Set PackageWithPath)
getStorePaths Config{..} = do
  fileExists <- test_e cfgDeclaredOutPaths
  if fileExists 
   then do
     results <- fmap parsePackageFromStorePathContents . readfile 
              $ cfgDeclaredOutPaths
     pkgs <- fmap catMaybes $ forM results $ \r -> do 
          case r of 
            Left msg -> echo_err ([st|Warning: `%s'|] msg) >> return Nothing
            Right pkg -> return $ Just pkg
     -- filter out pkgs that do not exist
     knownPkgs <- flip filterM pkgs $ \ (Pwp{ pwpPath = p }) -> do
       availability <- storePathAvailability p
       case availability of
            Available -> return True
            Buildable -> do 
              echo_err $ [st|Warning: store path is not available: %s. It has to be built. |] p
              return True
            Unknown -> do
              echo_err $  [st|Warning: store path is unknown: %s. Ignoring.|] p
              return False
     return (S.fromList knownPkgs)
   else do
    echo_err $ [st|Warning: installed store paths file does not exist (%s)|] 
               (cfgDeclaredOutPaths^.Utils.fpText)
    return (S.empty)

data Availability = Available | Buildable | Unknown

storePathAvailability :: Text -> Sh Availability
storePathAvailability p = do
  void $ cmd "nix-store" "-r" "--dry-run" p
  (availability . T.lines) =<< lastStderr
  where availability [] = return Available
        availability (t:_) | "don't know" `T.isInfixOf` t = return Unknown
        availability (t:_) | "these" `T.isPrefixOf` t = return Buildable
        availability ts = errorExit ([st|"storePathAvailability: unable to parse output:\n%s"|] 
                                    (T.unlines ts))

makeReport :: Bool -> Config -> Results -> PP.Doc
makeReport keepInstalled Config{..} r = 
  let (upds, install', removing') = 
        calculateUpdates (wanted r) (M.keysSet . rInstalled $ r)
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
     (if keepInstalled then PP.text "NOT removing (due to --keep-installed):" else "Removing:" )
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
  echo $ [st|\n* Clearing out cache profile \n   (%s)|] 
         (toTextIgnore . cfgDestProfile $ optCfg)
  nixCmdCfgExecute $ removePackages optCfg
  when optKeepInstalled $ do
    echo $ [st|\n* Installing from %s to cache profile \n   (%s)|] 
           (toTextIgnore . cfgProfile $ optCfg)
           (toTextIgnore . cfgDestProfile $ optCfg)
    pkgCmd "save currently installed" (installPackages (Just (cfgProfile optCfg))) 
           . map formatPackageWithPath
           . S.toList . M.keysSet
           . rInstalled $ r
  echo $ [st|\n* Installing packages from %s to cache profile \n   (%s)|] 
         (toTextIgnore . cfgDeclaredOutPaths $ optCfg)
         (toTextIgnore . cfgDestProfile $ optCfg)
  pkgCmd "install package list" (installPackages Nothing)
    . map formatPackageWithPath 
    . S.toList . M.keysSet 
    . wantedFromDeclared $ r
  pkgCmd "install store paths" installStorePaths (map pwpPath . S.toList . rStorePaths $ r)
  when (optCommand == Switch) $ do
    echo $ [st|\n* Switching profile %s|] (toTextIgnore . cfgProfile $ optCfg)
    nixCmdCfgExecute $ switchToNewPackages optCfg
  where pkgCmd source installCmd ps = 
          if null ps 
          then echo_err $ "nix-rebuild: Nothing to be installed from " <> source
          else nixCmdCfgExecute $ installCmd optCfg ps
        nixCmdCfgExecute = nixCmd_ . \n -> n { nixDryRun = optCommand == DryRun }

