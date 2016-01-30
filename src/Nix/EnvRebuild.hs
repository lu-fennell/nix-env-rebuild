{-# LANGUAGE NamedFieldPuns, NoImplicitPrelude, OverloadedStrings, ExtendedDefaultRules, RecordWildCards, NoMonomorphismRestriction, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
    
module Nix.EnvRebuild where

import BasicPrelude hiding (try, (</>), (<.>), FilePath, show, rem, takeWhile)
import Control.Applicative.QQ.ADo
import Control.Lens hiding (re)
import qualified Data.Text as T
import Data.Text.Lens
import Options.Applicative hiding (Parser)
import qualified Options.Applicative as Opt
import Prelude (show)
import Shelly hiding (path)
import Text.PrettyPrint (($$))
import qualified Text.PrettyPrint as PP
  

import qualified Data.Char as Char
import Formatting ((%))
import qualified Formatting as Fmt

import qualified Data.Set as S

import Data.Attoparsec.Text

import qualified Utils
import Nix.Types

default (Text)


---------------------------
-- Configuration and command line parsing
---------------------------

data Config = Config { cfgDeclaredPackages :: FilePath
                     , cfgKeepAroundProfile :: FilePath
                     , cfgDestProfile :: FilePath
                     , cfgCommand :: Command
                     , cfgVerbose :: Bool
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
               
getConfig :: Sh (Config)
getConfig = do
   declaredPackages <- readDeclaredPackages 
   keepAroundProfile <- readKeepAroundProfile 
   destProfile <- readDestProfile
   let flags :: Opt.Parser (Config)
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
                   cfgVerbose <- flag False True 
                     (long "verbose" <> short 'v'
                     <> help "echo nix commands and their output") 
                   cfgCommand <- subparser 
                     (  (Opt.command "dry-run" $ info (pure DryRun) 
                          (progDesc "Only show what would change"))
                     <> (Opt.command "build" $ info (pure Build) 
                          (progDesc "Build into cache profile but do not switch the target profile"))
                     <> (Opt.command "switch" $ info (pure Switch) 
                          (progDesc "Build packages into cache profile and switch the target profile")))
                     <|>
                     pure DryRun
                   
                   Config { cfgDeclaredPackages = cfgDeclaredPackages
                          , cfgVerbose = cfgVerbose 
                          , cfgKeepAroundProfile = cfgKeepAroundProfile
                          , cfgDestProfile = cfgDestProfile
                          , cfgCommand = cfgCommand
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
  cfg@Config{..} <- getConfig
  (if cfgVerbose then verbosely else silently) $ do
    r <- getResults cfg
    report cfg r
    when (cfgCommand /= DryRun) $ print_stdout True $ print_stderr True $ 
      doInstall cfg r
    echo $ "\n" <> finishMessage cfg 

  where finishMessage Config{..} = case cfgCommand of
          DryRun -> "Dry run. Not doing anyting."
          Build -> "Rebuild completed in profile "<> toTextIgnore cfgDestProfile
          Switch -> "Rebuild done"

        report cfg r = echo $ T.pack (PP.render (makeReport cfg r))

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

  
getResults :: Config -> Sh Results
getResults Config{..} = 
        Results <$> parseNix p_fromLocalQuery
                             (nixCmd $ (nixDefault NixQueryLocal)
                              { nixProfile = Just cfgKeepAroundProfile})
                <*> parseNix p_fromRemoteQuery
                             (nixCmd $ (nixDefault NixQueryRemote)
                              { nixFile = Just cfgDeclaredPackages
                              , nixProfile = Just cfgDestProfile })
                <*> parseNix p_fromLocalQuery
                             (nixCmd (nixDefault NixQueryLocal))
        where parseNix p c = S.fromList <$> parseNixOutput p c

---------------------------
-- Running nix-env
---------------------------

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
        assertNonEmpty [] = error $ "nixCmdStrings: empty selection list\n" ++ (show c)
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

doInstall :: Config -> Results -> Sh ()
doInstall cfg r = do
  -- TODO: clean up this if-then-else mess.. what commands to run should be specified on by the input to nixCmd
  nixCmdCfgExecute $ removePackages cfg
  pkgCmd "package list" installPackages $ map formatPackageWithPath $ S.toList $ wantedFromDeclared r
  pkgCmd "keep-around packages" installKeep $ map formatPackageWithPath $ S.toList $ wantedFromKept r
  when (cfgCommand cfg == Switch) $ do
    nixCmdCfgExecute $ switchToNewPackages cfg
  where pkgCmd source installCmd ps = 
          if null ps 
          then echo_err $ "nix-rebuild: Nothing to be installed from " <> source
          else nixCmdCfgExecute $ installCmd cfg ps
        nixCmdCfgExecute = nixCmd_ . \n -> n { nixDryRun = cfgCommand cfg == DryRun }

---------------------------
-- Parsers
---------------------------
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

        showT = view packed . show
                         

parsePackageWithPath :: Parser (Maybe (Package, StorePath, PkgStatus)) 
                     -> Text 
                     -> Either String (Maybe PackageWithPath)
parsePackageWithPath parser = over (_Right._Just) 
                         (\(n, p, s) -> Pwp { pwpPkg = parseVersionedPackage n 
                                            , pwpPath = p
                                            , pwpStatus = s
                                            })
                         . parseOnly parser
  

p_fromInstallAction :: Parser Text -> Parser Package
p_fromInstallAction prefix = skipSpace *> prefix *> skipSpace 
                             *> ("`" *> takeTill (=='\'')) 

p_fromInstalling, p_fromUninstalling :: Parser Package
p_fromInstalling =  p_fromInstallAction $ string "installing"
p_fromUninstalling =  p_fromInstallAction $ string "uninstalling"
                   

p_fromFileList :: Parser Text -> Parser Package
p_fromFileList p_packageName =
  skipSpace *>
  "/nix/store" 
  *> takeTill (== '-') *> skip (const True)
  *> p_packageName 

p_fromBuilding :: Parser Package
p_fromBuilding =
  p_fromFileList (view packed <$> manyTill anyChar ".drv")

p_fromFetching :: Parser Package 
p_fromFetching = (p_fromFileList takeText)
               
p_fromLocalQuery, p_fromRemoteQuery :: Parser (Maybe (Package, StorePath, PkgStatus))
p_fromLocalQuery = over (mapped._Just) 
                        (\(pkg, path) -> (pkg, path, Present)) 
                        p_fromQuery
p_fromRemoteQuery = do
  status <- p_status
  skipSpace
  mq <- p_fromQuery
  return $ over _Just (\(pkg, path) -> (pkg, path, status)) mq
  
p_status :: Parser PkgStatus
p_status = [ado|
             prebuiltCode <- code *> code *> code
             if prebuiltCode == 'S' then Prebuilt else Source
           |]
 where code = satisfy (inClass "-A-Z")

p_fromQuery :: Parser (Maybe (Package, StorePath))
p_fromQuery = do 
                name <- takeWhile (not . Char.isSpace)
                skipSpace
                path <- takeText
                guard (not . T.null $ path)
                guard (T.all (not . Char.isSpace) path)
                return $ Just (name, path)

p_wouldInstall, p_wouldRemove :: Parser (Maybe Package)
p_wouldInstall =  (Just <$> p_fromBuilding)
               <|> (Just <$> p_fromInstalling)
               <|> (Nothing <$ p_fromFetching) -- for now, ignore fetched packages
p_wouldRemove = Just <$> p_fromUninstalling

                

---------------------------
-- Utitilies               
---------------------------


fromJustE :: Text -> Maybe b -> b
fromJustE msg = fromMaybe (error $ msg^.unpacked)

(<$/!>) :: Text -> FilePath -> Sh FilePath
(<$/!>) var fp = (</> fp) . fromJustE msg <$> get_env var
  where msg = "Unable to read `" <> var <> "'"
     


