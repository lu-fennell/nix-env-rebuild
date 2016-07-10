{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, ExtendedDefaultRules, RecordWildCards #-}
{-# LANGUAGE RankNTypes, ExistentialQuantification #-}
{-# LANGUAGE PackageImports #-}
module Utils
       ( textReader
       , fileReader
       , getenv'
       , fromJustE
       , fromJustThrow
       , (<$/!>)
       -- | Lenses for FilePath
       , basename
       , directory
       , fpText
       , runStderr
       , maybeOpt
       , removeKeys
       , removeKeysOn
       , readSymbolicLink
       ) where

import BasicPrelude hiding (FilePath, (</>), (<.>))
import Shelly (Sh, get_env, toTextIgnore, runHandles, (-|-), run)
import Options.Applicative
import Options.Applicative.Types (readerAsk)
import Control.Lens hiding ((<.>))
import Control.Monad.Catch
import Data.Text.Lens
import Filesystem.Path.CurrentOS ((</>), (<.>), FilePath)
import qualified Filesystem.Path.CurrentOS as FilePath
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Text.IO (hGetContents)
import qualified System.Posix.Files as Unix
  
default (Text)

  
-- optparse-applicative

textReader :: ReadM Text
textReader = (view packed) <$> readerAsk
fileReader :: ReadM FilePath
fileReader = review fpText <$> textReader 
  
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
-- Misc
-- -------------------------------------------------------------------

maybeOpt :: Text -> Maybe Text -> [Text]
maybeOpt opt = maybe [] (\p -> [opt, p])

removeKeys :: (Eq a, Ord a) => Map a b -> Set a -> Map a b
removeKeys m s = removeKeysOn id m s

removeKeysOn :: (Eq a, Eq c, Ord a) => (a -> c) -> Map a b -> Set a -> Map a b
removeKeysOn proj m s = Set.foldl' (\m k -> Map.filterWithKey (\k' _ -> not (proj k' == proj k)) m) m s

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


fromJustE :: Text -> Maybe b -> b
fromJustE msg = fromMaybe (error $ msg^.unpacked)

fromJustThrow :: MonadThrow m => Either String a -> m a
fromJustThrow = either (throwM . userError) return
          

              
-- -------------------------------------------------------------------
-- System
-- -------------------------------------------------------------------

(<$/!>) :: Text -> FilePath -> Sh FilePath
(<$/!>) var fp = (</> fp) . review fpText . fromJustE msg <$> get_env var
  where msg = "Unable to read `" <> var <> "'"

readSymbolicLink :: FilePath -> Sh FilePath
readSymbolicLink p = liftIO (readlink p)
  where readlink p =   fmap (view (packed.(from fpText))) (Unix.readSymbolicLink (p^.(fpText.unpacked)))
  
