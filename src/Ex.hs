{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Ex where

import Prelude hiding (debug)
import qualified Data.ByteString.Char8 as B
import Foreign.C.Error
import Control.Lens
import System.Posix.Types
import System.Posix.Files
import System.Posix.IO
import Data.Foldable
import Data.IORef
import Control.Monad.State
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Bifunctor
import System.FilePath.Lens
import System.FilePath.Posix
import Data.Maybe
import Control.Applicative


import Tags
import Ops
import Data.IxSet as IS

import System.Fuse

type HT = ()

type TagPath = String

realFSRoot :: FilePath
realFSRoot = "/Users/chris/realFS"

data BetterFuseOps fh m = BetterFuseOps  {
        mfuseGetFileStat :: FilePath -> m (Either Errno FileStat),
        -- fuseReadSymbolicLink :: FilePath -> IO (Either Errno FilePath),
        -- fuseCreateDevice :: FilePath -> EntryType -> FileMode
        --                  -> DeviceID -> IO Errno,
        -- fuseCreateDirectory :: FilePath -> FileMode -> IO Errno,
        -- fuseRemoveLink :: FilePath -> IO Errno,
        -- fuseRemoveDirectory :: FilePath -> IO Errno,
        -- fuseCreateSymbolicLink :: FilePath -> FilePath -> IO Errno,
        -- fuseRename :: FilePath -> FilePath -> IO Errno,
        -- fuseCreateLink :: FilePath -> FilePath -> IO Errno,
        -- fuseSetFileMode :: FilePath -> FileMode -> IO Errno,
        -- fuseSetOwnerAndGroup :: FilePath -> UserID -> GroupID -> IO Errno,
        -- fuseSetFileSize :: FilePath -> FileOffset -> IO Errno,
        -- fuseSetFileTimes :: FilePath -> EpochTime -> EpochTime -> IO Errno,
        mfuseOpen :: FilePath -> OpenMode -> OpenFileFlags -> m (Either Errno fh),
        mfuseRead :: FilePath -> fh -> ByteCount -> FileOffset
                 -> m (Either Errno B.ByteString),
        -- fuseWrite :: FilePath -> fh -> B.ByteString -> FileOffset
        --           -> IO (Either Errno ByteCount),
        mfuseGetFileSystemStats :: String -> m (Either Errno FileSystemStats),
        -- fuseFlush :: FilePath -> fh -> IO Errno,
        -- fuseRelease :: FilePath -> fh -> IO (),
        -- fuseSynchronizeFile :: FilePath -> SyncType -> IO Errno,
        mfuseOpenDirectory :: FilePath -> m Errno,
        mfuseReadDirectory :: FilePath -> m (Either Errno [(FilePath, FileStat)]),
        -- fuseReleaseDirectory :: FilePath -> IO Errno,
        -- fuseSynchronizeDirectory :: FilePath -> SyncType -> IO Errno,
        -- fuseAccess :: FilePath -> Int -> IO Errno,
        mfuseInit :: m ()
        -- fuseDestroy :: IO ()
      }

newtype FilesM a = FilesM {runFilesM :: StateT TagMap IO a }
    deriving newtype (Functor, Applicative, Monad, MonadState TagMap, MonadIO)

transformOps :: BetterFuseOps fh FilesM -> IO (FuseOperations fh)
transformOps BetterFuseOps{..} = do
    filesRef <- newFiles
    let wrap1 act a = wrapState filesRef $ act a
    let wrap2 act a b = wrapState filesRef $ act a b
    let wrap3 act a b c = wrapState filesRef $ act a b c
    let wrap4 act a b c d = wrapState filesRef $ act a b c d
    return $
        defaultFuseOps { fuseGetFileStat = wrap1 mfuseGetFileStat
                       , fuseOpen        = wrap3 mfuseOpen
                       , fuseRead        = wrap4 mfuseRead
                       , fuseOpenDirectory = wrap1 mfuseOpenDirectory
                       , fuseReadDirectory = wrap1 mfuseReadDirectory
                       , fuseGetFileSystemStats = wrap1 mfuseGetFileSystemStats
                       }

wrapState :: IORef TagMap -> FilesM a -> IO a
wrapState ref m = do
    files <- readIORef ref
    (a, files') <- flip runStateT files $ runFilesM m
    writeIORef ref files'
    return a

newFiles :: IO (IORef TagMap)
newFiles = newIORef starterFS

main :: IO ()
main = do
    fsOps <- transformOps helloFSOps
    fuseMain fsOps (\e -> debugS "Error" e >> defaultExceptionHandler e)

helloFSOps :: BetterFuseOps HT FilesM
helloFSOps = BetterFuseOps { mfuseGetFileStat = helloGetFileStat
                           , mfuseOpen        = fileOpen
                           , mfuseRead        = helloRead
                           , mfuseOpenDirectory = openTag
                           , mfuseReadDirectory = taggedWith
                           , mfuseGetFileSystemStats = helloGetFileSystemStats
                           , mfuseInit = helloInit
                           }


helloInit :: FilesM ()
helloInit = debug "Init!"

helloPath :: FilePath
helloPath = "/hello"

helloGetFileStat :: FilePath -> FilesM (Either Errno FileStat)
helloGetFileStat "/" = do
    debugS "reading file stat" "/"
    ctx <- liftIO getFuseContext
    debug "success"
    return . Right $ (dirStat ctx)
helloGetFileStat filePath = do
    debugS "reading file stat" filePath
    ctx <- liftIO getFuseContext
    fileFromPath filePath >>= \case
        Just f -> return $ Right (statFile ctx f)
        Nothing -> return $ Left eNOENT

openTag :: TagPath -> FilesM Errno
openTag tagPath = do
    debugS "opening tag" tagPath
    exists <- isJust <$> fileFromPath tagPath
    if exists then debug "Success" >> return eOK
              else debug "Failure" >> return eNOENT

taggedWith :: TagPath -> FilesM (Either Errno [(FilePath, FileStat)])
taggedWith "/" = do
    ctx <- liftIO getFuseContext
    files <- filesForTags "/"
    debugS "SPECIAL" (nameStat ctx <$> IS.toList files)
    return . Right $ [(".", dirStat ctx), ("..", dirStat ctx)] <> (nameStat ctx <$> IS.toList files)
taggedWith tagPath = do
    files <- filesForTags tagPath
    debug ("found tagged with " <> tagPath <> ": " <> show files)
    ctx <- liftIO $ getFuseContext
    let dirs = defaultDirs ctx <> (nameStat ctx <$> IS.toList files)
    debugS "listing dirs" dirs
    pure . Right $  dirs
  where
    defaultDirs ctx = [(".", dirStat ctx), ("..", dirStat ctx)]


fileOpen :: FilePath -> OpenMode -> OpenFileFlags -> FilesM (Either Errno HT)
fileOpen path mode _flags = do
    debugS "opening: " path
    file <- fileFromPath path
    case (file, mode) of
        (Nothing, _) -> return (Left eNOENT)
        (_,  ReadOnly) -> return (Left eACCES)
        _ -> return (Right ())

helloRead :: FilePath -> HT -> ByteCount -> FileOffset -> FilesM (Either Errno B.ByteString)
helloRead  path _ byteCount offset = do
    debugS "loading " path
    debugS "loading from" (realFSRoot </> (path ^. filename))
    liftIO $ (Right <$> getBytes) <|> pure (Left eNOENT)
  where
    getBytes = do
        fileContents <- BS.readFile (realFSRoot </> (path ^. filename))
        return $ BS.take (fromIntegral byteCount) $ BS.drop (fromIntegral offset) fileContents

-- helloRead path _ byteCount offset
--     | path == helloPath =
--         return $ Right $ B.take (fromIntegral byteCount) $ B.drop (fromIntegral offset) helloString
--     | otherwise         = return $ Left eNOENT

helloGetFileSystemStats :: String -> FilesM (Either Errno FileSystemStats)
helloGetFileSystemStats str =
  return $ Right $ FileSystemStats
    { fsStatBlockSize = 512
    , fsStatBlockCount = 1
    , fsStatBlocksFree = 1
    , fsStatBlocksAvailable = 1
    , fsStatFileCount = 5
    , fsStatFilesFree = 10
    , fsStatMaxNameLength = 255
    }

deriving instance Eq OpenMode

starterFS :: IxSet TFile
starterFS =
    IS.fromList [ newTag "photos" []
                , newTag "vacation" [Tag "photos"]
                , newFile "sandy-beach.png" [Tag "photos", Tag "vacation"]
                , newTag "/" []
                ]
