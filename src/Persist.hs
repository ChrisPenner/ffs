{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module Persist where

import Data.SafeCopy
import Data.Serialize hiding (get)
import Types
import System.Directory
import Control.Lens
import System.FilePath.Posix
import qualified Data.ByteString as BS
import Util
import Control.Monad.IO.Class

import Polysemy
import Polysemy.IO
import Polysemy.Input
import Polysemy.State

loadIndex :: MonadIO m => m TagMap
loadIndex = do
    tagsFilePath <- getPersistenceDir <&> (</> "tags")
    tagMapBS <- liftIO $ BS.readFile tagsFilePath
    case runGet safeGet tagMapBS of
        Left err -> debugS ("Error loading tags file from " <> tagsFilePath) err >> return mempty
        Right tagmap -> return tagmap

persistIndex :: (Member (Input Env) r, Member (State TagMap) r, Member (Embed IO) r)
             => Sem r ()
persistIndex = do
    tagMap <- get
    tagsFilePath <- input <&> view (persistDir . to (</> "tags"))
    embed $ BS.writeFile tagsFilePath (runPut (safePut tagMap))

getPersistenceDir :: MonadIO m => m FilePath
getPersistenceDir = liftIO $ do
    persistenceDir <- getXdgDirectory XdgData  "ffs"
    let createParents = False
    createDirectoryIfMissing createParents persistenceDir
    return persistenceDir

