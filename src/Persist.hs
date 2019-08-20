{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module Persist where

import Data.SafeCopy
import Data.Serialize hiding (get)
import Types
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.IO.Class
import System.Directory
import Control.Lens
import System.FilePath.Posix
import qualified Data.ByteString as BS
import Util

loadIndex :: (MonadIO m) => m TagMap
loadIndex = do
    tagsFilePath <- getPersistenceDir <&> (</> "tags")
    tagMapBS <- liftIO $ BS.readFile tagsFilePath
    case runGet safeGet tagMapBS of
        Left err -> debugS ("Error loading tags file from " <> tagsFilePath) err >> return mempty
        Right tagmap -> return tagmap

persistIndex :: (MonadReader Env m, MonadState TagMap m, MonadIO m) => m ()
persistIndex = do
    tagMap <- get
    tagsFilePath <- view (persistDir . to (</> "tags"))
    liftIO $ BS.writeFile tagsFilePath (runPut (safePut tagMap))

getPersistenceDir :: MonadIO m => m FilePath
getPersistenceDir = liftIO $ do
    persistenceDir <- getXdgDirectory XdgData  "ffs"
    let createParents = False
    createDirectoryIfMissing createParents persistenceDir
    return persistenceDir

