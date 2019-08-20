{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Ops where

import Control.Lens
import Control.Monad.State
import Control.Monad.Reader
import System.FilePath.Lens
import System.FilePath.Posix
import Data.IxSet
import System.Fuse
import System.Posix.Types
import System.Posix.Files
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B

import Types
import Util

getRealFilePath :: MonadReader Env m => TFile -> m FilePath
getRealFilePath tfile = do
    fsRoot <- view realRoot
    return $ fsRoot </> (tfile ^. name . to unName)

getRealFilePathFromTagPath :: MonadReader Env m => FilePath -> m FilePath
getRealFilePathFromTagPath tagPath = do
    fsRoot <- view realRoot
    return $ fsRoot </> (tagPath ^. filename)


filesForTags :: (MonadState TagMap m, MonadIO m) =>  FilePath -> m TagMap
filesForTags tagPath = do
    let tags = splitDirectories tagPath
    tagMap <- get
    return $ tagMap @* (Tag <$> tags)

fileFromPath :: (MonadState TagMap m, MonadIO m) => FilePath -> m (Maybe TFile)
fileFromPath "/" = do
    debugS "fetching file for path" "/"
    result <- getOne . getEQ (Name "/") <$> get
    debugS "got" result
    return result

fileFromPath filePath = do
    debugS "fetching file for path" filePath
    result <- getOne . getEQ (filePath ^. filename . to Name) <$> filesForTags (filePath ^. directory)
    debugS "got" result
    return result

nameStat :: (MonadReader Env m, MonadIO m) => FuseContext -> TFile -> m (String, FileStat)
nameStat ctx f = (view (name . to unName) f,) <$> statFile ctx f


-- statFile ::  FuseContext -> TFile -> FileStat
-- statFile ctx (_fileType -> TypeTag) = dirStat ctx
-- statFile ctx (_fileType -> TypeFile) = fileStat ctx


statFile :: (MonadReader Env m, MonadIO m) => FuseContext -> TFile -> m FileStat
statFile ctx (_fileType -> TypeTag) = pure $ defaultDirStat ctx
statFile ctx file = do
    rootPath <- view realRoot
    fileStats <- liftIO $ getFileStatus (rootPath </> file ^. name . to unName)
    return $ FileStat
             { statEntryType        = statusType fileStats
             , statFileMode         = fileMode fileStats
             , statLinkCount        = linkCount fileStats
             , statFileOwner        = fileOwner fileStats
             , statFileGroup        = fileGroup fileStats
             , statSpecialDeviceID  = specialDeviceID fileStats
             , statFileSize         = fileSize fileStats
             , statBlocks           = fromIntegral . max 1 . ceiling $ (fromIntegral (fileSize fileStats) / 512) -- size in 512 byte blocks
             , statAccessTime       = accessTime fileStats
             , statModificationTime = modificationTime fileStats
             , statStatusChangeTime = statusChangeTime fileStats
             }

statusType :: FileStatus -> EntryType
statusType fs
  | isDirectory fs = Directory
  | isRegularFile fs = RegularFile
  | otherwise = error "unknown filetype"

defaultDirStat :: FuseContext -> FileStat
defaultDirStat ctx = FileStat { statEntryType = Directory
                       , statFileMode = foldr1 unionFileModes
                                          [ ownerReadMode
                                          , ownerExecuteMode
                                          , groupReadMode
                                          , groupExecuteMode
                                          , otherReadMode
                                          , otherExecuteMode
                                          ]
                       , statLinkCount = 2
                       , statFileOwner = fuseCtxUserID ctx
                       , statFileGroup = fuseCtxGroupID ctx
                       , statSpecialDeviceID = 0
                       , statFileSize = 4096 -- size in bytes
                       , statBlocks = 1 -- size in 512-byte blocks
                       , statAccessTime = 0
                       , statModificationTime = 0
                       , statStatusChangeTime = 0
                       }

-- fileStat :: FuseContext -> FileStat
-- fileStat ctx = FileStat { statEntryType = RegularFile
--                         , statFileMode = foldr1 unionFileModes
--                                            [ ownerReadMode
--                                            , groupReadMode
--                                            , otherReadMode
--                                            ]
--                         , statLinkCount = 1
--                         , statFileOwner = fuseCtxUserID ctx
--                         , statFileGroup = fuseCtxGroupID ctx
--                         , statSpecialDeviceID = 0
--                         , statFileSize = fromIntegral $ B.length "" -- Do a real filesize
--                         , statBlocks = 1
--                         , statAccessTime = 0
--                         , statModificationTime = 0
--                         , statStatusChangeTime = 0
--                         }
