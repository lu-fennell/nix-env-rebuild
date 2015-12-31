{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, ExtendedDefaultRules, RecordWildCards #-}
{-# LANGUAGE RankNTypes, ExistentialQuantification #-}
{-# LANGUAGE PackageImports #-}
module Nix.Utils
       ( textReader
       , fileReader
       , getenv'
       , override
       , (<--)
       , Update'()
       , PackageVersion
       , splitPackage
       -- | Lenses for FilePath
       , basename
       , directory
       , fpText
       , parseNixPatch
       , applyNixPatch
       , NixPatch(..)
       , runStderr
       , maybeOpt
       ) where

import BasicPrelude hiding (FilePath, (</>), (<.>))
import Shelly (Sh, get_env, toTextIgnore, runHandles, (-|-), run)
import Options.Applicative hiding ((&))
import Options.Applicative.Types (readerAsk)
import Data.Traversable (sequenceA)
import Control.Lens hiding ((<.>))
import Data.Text.Lens
import Filesystem.Path.CurrentOS ((</>), (<.>), FilePath)
import qualified Filesystem.Path.CurrentOS as FilePath
import qualified Data.Text as Text
import qualified Data.Char as Char
import "mtl" Control.Monad.State
import Data.Text.IO (hGetContents)
  
default (Text)

  
-- optparse-applicative

textReader :: ReadM Text
textReader = (view packed) <$> readerAsk
fileReader :: ReadM FilePath
fileReader = review fpText <$> textReader 
-- textOption mods = -- fmap (view packed) (strOption mods)
--   nullOption mods <> 

-- fileOption mods = fmap fromText (textOption mods)

newtype Update' a = Update' { _runUpdate :: a -> Parser (State a ()) }
                    
instance Monoid (Update' a) where
  mempty = Update' $ \x -> pure (return ())
  mappend (Update' f) (Update' g) = Update' $ \x -> liftA2 (>>) (f x) (g x)
  
(<--) :: Show b => Lens' a b -> (ReadM b , Mod OptionFields b) -> Update' a
l <-- (r,p) = Update' $ \x -> (l .=) <$> option r (p <> value (view l x) <> showDefault)
  
  
                          
override :: a -> Update' a -> Parser a
override x u = flip execState x <$> (_runUpdate u x)
             

  
-------------------
-- Shelly

-- | Get environment variable with informative error message on failure
getenv' :: Text -> Sh Text
getenv' v =
  fromMaybe (error "environment variable " <> v <> " not found(??)")
  <$> get_env v
  
runStderr :: FilePath -> [Text] -> Sh Text
runStderr p args = runHandles p args [] $ \_ _ h -> liftIO (hGetContents h) -|- run "cat" []


-- -------------------------------------------------------------------
-- nix package versions
-- -------------------------------------------------------------------
type PackageVersion = Text

splitPackage :: Text -> (Text, Text)
splitPackage p = (name, versionString)
  where (name, versionString) = over both (Text.intercalate "-") 
                                     (splitFirstNonLetter (Text.splitOn "-" p))
        splitFirstNonLetter :: [Text] -> ([Text],[Text])
        splitFirstNonLetter xs = fromMaybe (xs,[])
                                           (splitAt <$> findIndex firstNonLetter xs <*> pure xs)
        firstNonLetter :: Text -> Bool
        firstNonLetter t = isJust $ guard . not . Char.isLetter =<< preview _head t
  

-- -------------------------------------------------------------------
-- Misc
-- -------------------------------------------------------------------

maybeOpt :: Text -> Maybe FilePath -> [Text]
maybeOpt opt = maybe [] (\p -> [opt, toTextIgnore p])

-- -------------------------------------------------------------------
-- Filepath lenses
-- -------------------------------------------------------------------
-- TODO: write some tests

basename :: Lens' FilePath FilePath
basename = lens (FilePath.basename) (\b p -> let noExtension = FilePath.directory p </> b 
                                             in fromMaybe noExtension $ (noExtension <.>) <$> FilePath.extension p)
         
directory :: Lens' FilePath FilePath
directory = lens FilePath.directory (\d p -> d </> FilePath.filename p)
          
fpText :: Iso' FilePath Text
fpText = iso toTextIgnore FilePath.fromText
          

  
                
-- -------------------------------------------------------------------
-- nixpatch
-- -------------------------------------------------------------------
-- A very rudimentary format to patch derivations. Informally:
--   #%declare [args..]
--   #%declare ...
--   [derivation-attributes] 
-- 
-- the declared arguments are added to the arguments of the patched
-- derivation and the derivation-attributes are pasted just before the
-- meta attribute.

-- !!!! This is very brittle and is only tested for the output that
-- !!!! cabal2nix (currently) produces. A real parser should be used
-- !!!! for this.
                
data NixPatch = NixPatch { npDeclared :: [Text], npBody :: Text }
  deriving Show

type ParseErrorMsg = Text
           
parseNixPatch :: Text -> NixPatch
parseNixPatch input = NixPatch declarations (Text.unlines bodyLines)
  where (declarationLines, bodyLines) = partition (declarePrefix `Text.isPrefixOf`) inputlines
        declarations = [ decl | Just line <- map (Text.stripPrefix declarePrefix) declarationLines
                              , decl <- Text.words line
                              ]
        inputlines = Text.lines input
        declarePrefix = "#%declare " 

applyNixPatch :: NixPatch -> Text -> Either ParseErrorMsg Text
applyNixPatch NixPatch{..} input = do
  args <- extractArgs inputlines
  return $ Text.unlines (map (patchLine args) inputlines)
  where inputlines = Text.lines input
        patchLine oldargs l | l == oldargs =
          let prefix = Text.dropEnd 1 (Text.dropWhileEnd (/= '}') l)
              lastline = if BasicPrelude.null npDeclared 
                         then " }:"
                         else ", "<>(Text.intercalate ", " npDeclared)<>" }:"
          in prefix <> lastline
        patchLine oldargs l | "description =" `Text.isPrefixOf` (Text.strip l) =
          npBody <> "\n" <> l
        patchLine _ l       | otherwise = l
        extractArgs ls = do
          let stripped = dropWhile (not . ("}:" `Text.isSuffixOf`)) ls
          case stripped of
            [] -> fail ("Could not find argument list in\n" <> Text.unpack input)
            (args:_) -> return args
                                           

          
                                                
                                                
