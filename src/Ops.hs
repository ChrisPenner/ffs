{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
module Ops where

import Control.Lens
import Tags
import Control.Monad.State
import System.FilePath.Lens
import System.FilePath.Posix
import Data.IxSet
import System.Fuse
import System.Posix.Types
import System.Posix.Files
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B

filesForTags :: (MonadState TagMap m, MonadIO m) =>  FilePath -> m TagMap
filesForTags tagPath = do
    let tags = splitDirectories tagPath
    tagMap <- get
    debugReturn ("Files for tag path: " <> tagPath) $ tagMap @* (Tag <$> tags)

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

debug :: MonadIO m => String -> m ()
debug s = do
    liftIO $ appendFile "/Users/chris/fuse.log" (s <> "\n")

debugReturn :: (Show a, MonadIO m) => String -> a -> m a
debugReturn s a = do
    liftIO $ appendFile "/Users/chris/fuse.log" (s <> ": " <> show a <> "\n")
    return a

debugS :: (Show a, MonadIO m) => String -> a -> m ()
debugS s a = do
    liftIO $ appendFile "/Users/chris/fuse.log" (s <> ": " <> show a <> "\n")

nameStat :: FuseContext -> TFile -> (String, FileStat)
nameStat ctx f = (view (name . to unName) f, statFile ctx f)

statFile ::  FuseContext -> TFile -> FileStat
statFile ctx (_fileType -> TypeTag) = dirStat ctx
statFile ctx (_fileType -> TypeFile) = fileStat ctx


dirStat :: FuseContext -> FileStat
dirStat ctx = FileStat { statEntryType = Directory
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
                       , statFileSize = 4096
                       , statBlocks = 1
                       , statAccessTime = 0
                       , statModificationTime = 0
                       , statStatusChangeTime = 0
                       }

fileStat :: FuseContext -> FileStat
fileStat ctx = FileStat { statEntryType = RegularFile
                        , statFileMode = foldr1 unionFileModes
                                           [ ownerReadMode
                                           , groupReadMode
                                           , otherReadMode
                                           ]
                        , statLinkCount = 1
                        , statFileOwner = fuseCtxUserID ctx
                        , statFileGroup = fuseCtxGroupID ctx
                        , statSpecialDeviceID = 0
                        , statFileSize = fromIntegral $ B.length helloString -- Do a real filesize
                        , statBlocks = 1
                        , statAccessTime = 0
                        , statModificationTime = 0
                        , statStatusChangeTime = 0
                        }


helloString :: B.ByteString
helloString = B.pack "Hello World, HFuse!\n"

