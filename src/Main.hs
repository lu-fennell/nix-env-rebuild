{-# LANGUAGE NamedFieldPuns, NoImplicitPrelude, OverloadedStrings, ExtendedDefaultRules, RecordWildCards, NoMonomorphismRestriction, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
    
module Main where

import BasicPrelude hiding (try, (</>), (<.>), FilePath, show, rem, takeWhile)
import Control.Applicative.QQ.ADo
import qualified Data.Text as T
import Options.Applicative hiding (Parser)
import qualified Options.Applicative as Opt
import Prelude (show)
import Shelly hiding (path)
import Text.PrettyPrint (($$))
import qualified Text.PrettyPrint as PP
  

import Formatting ((%))
import qualified Formatting as Fmt

import qualified Data.Set as S

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
readDeclaredPackages, readKeepAroundProfile, readDestProfile :: Sh FilePath
readDeclaredPackages = "HOME" <$/!> ".nixpkgs/packages.nix" 
readKeepAroundProfile = "NIX_USER_PROFILE_DIR" <$/!> "keep-around"
readDestProfile = "NIX_USER_PROFILE_DIR" <$/!> "nix-rebuild-cache"
                      
tmpProfileName :: Text
tmpProfileName = "tmp-nix-rebuild-profile"
               
getConfig :: Sh (Opt)
getConfig = do
   declaredPackages <- readDeclaredPackages 
   keepAroundProfile <- readKeepAroundProfile 
   destProfile <- readDestProfile
   let flags :: Opt.Parser (Opt)
       flags = [ado|
                   cfgDeclaredPackages <- Opt.option Utils.fileReader 
                     (long "packages"
                      <> metavar "FILE"
                      <> value declaredPackages <> showDefault
                      <> help "File declaring the set of packages that should be installed (as a nix list)")
                   cfgKeepAroundProfile <- Opt.option Utils.fileReader 
                     (long "keep-profile"
                     <> metavar "DIR"
                     <> value keepAroundProfile <> showDefault
                     <> help "Profile with installed packages. \
                             \These packages take precedence over packages with the same name in nixpkgs")
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
                   
                   Opt {  optCfg = Config { cfgDeclaredPackages = cfgDeclaredPackages
                          , cfgKeepAroundProfile = cfgKeepAroundProfile
                          , cfgDestProfile = cfgDestProfile }
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
    when (optCommand /= DryRun) $ print_stdout True $ print_stderr True $ 
      doInstall opt r
    echo $ "\n" <> finishMessage opt

  where finishMessage Opt{..} = case optCommand of
          DryRun -> "Dry run. Not doing anyting."
          Build -> "Rebuild completed in profile "<> toTextIgnore (cfgDestProfile optCfg)
          Switch -> "Rebuild done"

        report cfg r = echo $ T.pack (PP.render (makeReport cfg r))

getResults :: Config -> Sh Results
getResults Config{..} = 
        Results <$> parseNix P.fromLocalQuery
                             (nixCmd $ (nixDefault NixQueryLocal)
                              { nixProfile = Just cfgKeepAroundProfile})
                <*> parseNix P.fromRemoteQuery
                             (nixCmd $ (nixDefault NixQueryRemote)
                              { nixFile = Just cfgDeclaredPackages
                              , nixProfile = Just cfgDestProfile })
                <*> parseNix P.fromLocalQuery
                             (nixCmd (nixDefault NixQueryLocal))
        where parseNix p c = S.fromList <$> P.parseNixOutput p c


makeReport :: Config -> Results -> PP.Doc
makeReport Config{..} r = 
  let (upds, install', removing') = 
        filterUpds (installing r) (removing r)
      versionUpdates = S.filter isStrictUpdate upds
      reinstalls = S.map newPackage $ upds S.\\ versionUpdates
      sourceReinstalls = S.filter (\Pwp{pwpStatus} -> pwpStatus == Source) 
                                  reinstalls
  in PP.text "Updating:" $$ PP.empty
     $$ PP.nest 2 (formatSet formatUpd versionUpdates)
     $$ PP.char ' ' $$
     PP.text "Adding:" $$ PP.empty
     $$ PP.nest 2 (formatSet formatPackageWithPath install')
     $$ PP.char ' ' $$
     PP.text "Reinstalling from source:" $$ PP.empty
     $$ PP.nest 2 (formatSet formatPackageWithPath sourceReinstalls)
     $$ PP.char ' ' $$
     PP.text "Removing:" 
     $$ PP.nest 2 (formatSet formatPackageWithPath removing')
     $$
     pptext (Fmt.sformat ("Forcing updates (from "%Fmt.stext%"):") 
                         (toTextIgnore cfgKeepAroundProfile))
     $$ PP.nest 2 (formatSet formatUpd (keptUpdates r))
     $$
     pptext (Fmt.sformat ( "Ignored updates and reinstalls \
                            \(from "%Fmt.stext%"):") 
                          (toTextIgnore cfgKeepAroundProfile))
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
  -- TODO: clean up this if-then-else mess.. what commands to run should be specified on by the input to nixCmd
  nixCmdCfgExecute $ removePackages optCfg
  pkgCmd "package list" installPackages $ map formatPackageWithPath $ S.toList $ wantedFromDeclared r
  pkgCmd "keep-around packages" installKeep $ map formatPackageWithPath $ S.toList $ wantedFromKept r
  when (optCommand == Switch) $ do
    nixCmdCfgExecute $ switchToNewPackages optCfg
  where pkgCmd source installCmd ps = 
          if null ps 
          then echo_err $ "nix-rebuild: Nothing to be installed from " <> source
          else nixCmdCfgExecute $ installCmd optCfg ps
        nixCmdCfgExecute = nixCmd_ . \n -> n { nixDryRun = optCommand == DryRun }


