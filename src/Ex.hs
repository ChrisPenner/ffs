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
{-# LANGUAGE PackageImports #-}
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
import Control.Monad.Reader
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Bifunctor
import System.FilePath.Lens
import System.FilePath.Posix
import System.Environment
import System.Exit
import System.IO
import System.Directory
import Data.Maybe
import Control.Applicative
import Data.IxSet as IS
import Data.Functor
import "unix-bytestring" System.Posix.IO.ByteString (fdPread, fdPwrite)

import Persist
import Types
import Ops
import Util

import System.Fuse

type FfsFd = Fd

type TagPath = String

data BetterFuseOps fh m = BetterFuseOps  {
        mfuseGetFileStat :: FilePath -> m (Either Errno FileStat),
        -- mfuseReadSymbolicLink :: FilePath -> m (Either Errno FilePath),
        -- mfuseCreateDevice :: FilePath -> EntryType -> FileMode
        --                  -> DeviceID -> m Errno,
        mfuseCreateDirectory :: FilePath -> FileMode -> m Errno,
        -- mfuseRemoveLink :: FilePath -> m Errno,
        -- mfuseRemoveDirectory :: FilePath -> m Errno,
        -- mfuseCreateSymbolicLink :: FilePath -> FilePath -> m Errno,
        -- mfuseRename :: FilePath -> FilePath -> m Errno,
        -- mfuseCreateLink :: FilePath -> FilePath -> m Errno,
        -- mfuseSetFileMode :: FilePath -> FileMode -> m Errno,
        -- mfuseSetOwnerAndGroup :: FilePath -> UserID -> GroupID -> m Errno,
        mfuseSetFileSize :: FilePath -> FileOffset -> m Errno,
        -- mfuseSetFileTimes :: FilePath -> EpochTime -> EpochTime -> m Errno,
        mfuseOpen :: FilePath -> OpenMode -> OpenFileFlags -> m (Either Errno fh),
        mfuseRead :: FilePath -> fh -> ByteCount -> FileOffset
                 -> m (Either Errno B.ByteString),
        mfuseWrite :: FilePath -> fh -> B.ByteString -> FileOffset
                  -> m (Either Errno ByteCount),
        mfuseGetFileSystemStats :: String -> m (Either Errno FileSystemStats),
        mfuseFlush :: FilePath -> fh -> FilesM Errno,
        mfuseRelease :: FilePath -> fh -> FilesM (),
        mfuseSynchronizeFile :: FilePath -> SyncType -> FilesM Errno,
        mfuseOpenDirectory :: FilePath -> m Errno,
        mfuseReadDirectory :: FilePath -> m (Either Errno [(FilePath, FileStat)]),
        -- mfuseReleaseDirectory :: FilePath -> FilesM Errno,
        -- mfuseSynchronizeDirectory :: FilePath -> SyncType -> FilesM Errno,
        -- mfuseAccess :: FilePath -> Int -> FilesM Errno,
        mfuseInit :: m (),
        mfuseDestroy :: m ()
      }

newtype FilesM a = FilesM {runFilesM :: ReaderT Env (StateT TagMap IO) a }
    deriving newtype (Functor, Applicative, Monad, MonadState TagMap, MonadIO, MonadReader Env)

transformOps :: String -> BetterFuseOps fh FilesM -> IO (FuseOperations fh)
transformOps rootFS BetterFuseOps{..} = do
    filesRef <- loadIndex >>= newIORef
    let wrap act = wrapState rootFS filesRef $ act
    let wrap1 act = wrap . act
    let wrap2 act = wrap1 . act
    let wrap3 act = wrap2 . act
    let wrap4 act = wrap3 . act
    return $
        defaultFuseOps { fuseGetFileStat = wrap1 mfuseGetFileStat
                       , fuseOpen        = wrap3 mfuseOpen
                       , fuseRead        = wrap4 mfuseRead
                       , fuseOpenDirectory = wrap1 mfuseOpenDirectory
                       , fuseReadDirectory = wrap1 mfuseReadDirectory
                       , fuseGetFileSystemStats = wrap1 mfuseGetFileSystemStats
                       , fuseInit = wrap mfuseInit
                       , fuseDestroy = wrap mfuseDestroy
                       , fuseWrite = wrap4 mfuseWrite
                       , fuseSetFileSize = wrap2 mfuseSetFileSize
                       , fuseFlush = wrap2 mfuseFlush
                       , fuseRelease = wrap2 mfuseRelease
                       , fuseSynchronizeFile = wrap2 mfuseSynchronizeFile
                       , fuseCreateDirectory = wrap2 mfuseCreateDirectory
                       }

wrapState :: String -> IORef TagMap -> FilesM a -> IO a
wrapState rootFS ref m = do
    files <- readIORef ref
    persistDir <- getPersistenceDir
    let env = Env rootFS persistDir
    (a, files') <- flip runStateT files . flip runReaderT env $ runFilesM m
    writeIORef ref files'
    return a

newFiles :: IO (IORef TagMap)
newFiles = newIORef starterFS

main :: IO ()
main = do
    prog <- getProgName
    (rootfs, args) <- getArgs >>= \case
        (rootfs:args) -> do
            exists <- doesDirectoryExist rootfs
            when (not exists) $ hPutStrLn stderr ("Bad file root: " <> rootfs) >> exitFailure
            return (rootfs, args)
        args -> hPutStrLn stderr "usage: ffs <files> <mount point>" >> exitFailure
    fsOps <- transformOps rootfs helloFSOps
    fuseRun prog args fsOps (\e -> debugS "Error" e >> defaultExceptionHandler e)

helloFSOps :: BetterFuseOps FfsFd FilesM
helloFSOps = BetterFuseOps { mfuseGetFileStat = ffsGetFileStat
                           , mfuseOpen        = ffsOpen
                           , mfuseRead        = ffsReadFile
                           , mfuseOpenDirectory = ffsOpenDirectory
                           , mfuseReadDirectory = ffsReadDirectory
                           , mfuseGetFileSystemStats = getFileSystemStats
                           , mfuseInit = initFFS
                           , mfuseDestroy = destroyFFS
                           , mfuseWrite = ffsWriteFile
                           , mfuseSetFileSize = ffsSetFileSize
                           , mfuseFlush = ffsFlush
                           , mfuseRelease = ffsRelease
                           , mfuseSynchronizeFile = ffsSynchronizeFile
                           , mfuseCreateDirectory = ffsCreateDirectory
                           }

-- Fix this dumb assumption
initFFS :: FilesM ()
initFFS = return ()

-- Fix this dumb assumption
destroyFFS :: FilesM ()
destroyFFS = persistIndex

ffsGetFileStat :: FilePath -> FilesM (Either Errno FileStat)
ffsGetFileStat filePath = do
    debugS "reading file stat" filePath
    ctx <- liftIO getFuseContext
    fileFromPath filePath >>= \case
        Just f@(view fileType -> TypeTag)  -> return . Right $ defaultDirStat ctx
        Just f@(view fileType -> TypeFile) -> Right <$> statFile ctx f
        Nothing -> do
            return $ Left eNOENT
            -- return . Right $ defaultDirStat ctx

ffsOpenDirectory :: TagPath -> FilesM Errno
ffsOpenDirectory tagPath = do
    debugS "opening tag" tagPath
    exists <- isJust <$> fileFromPath tagPath
    if exists
        then debug "Success" >> return eOK
        else debug "Failure" >> return eNOENT

ffsReadDirectory :: TagPath -> FilesM (Either Errno [(FilePath, FileStat)])
ffsReadDirectory tagPath = do
    files <- filesForTags tagPath
    debug ("found tagged with " <> tagPath <> ": " <> show files)
    ctx <- liftIO $ getFuseContext
    foundFiles <- traverse (nameStat ctx) (IS.toList files)
    let dirs = defaultDirs ctx <> foundFiles
    debugS "listing dirs" dirs
    pure . Right $ dirs
  where
    defaultDirs ctx = [(".", defaultDirStat ctx), ("..", defaultDirStat ctx)]


ffsOpen :: FilePath -> OpenMode -> OpenFileFlags -> FilesM (Either Errno Fd)
ffsOpen path mode flags = do
    debugS "opening: " path
    realFilePathM <- fileFromPath path >>= traverse getRealFilePath
    debugS "file found" realFilePathM
    case realFilePathM of
        (Just realFilePath) -> do
            Right <$> (liftIO $ openFd realFilePath mode Nothing flags)
        Nothing -> return (Left eNOENT)

ffsReadFile :: FilePath -> FfsFd -> ByteCount -> FileOffset -> FilesM (Either Errno B.ByteString)
ffsReadFile  path fd byteCount offset = do
    debugS "readFile " path
    liftIO $ (Right <$> fdPread fd byteCount offset) -- TODO add better error handling

ffsWriteFile :: FilePath -> FfsFd -> BS.ByteString -> FileOffset -> FilesM (Either Errno ByteCount)
ffsWriteFile  path fd contents offset = do
    debugS "writeFile " path
    liftIO $ (Right <$> fdPwrite fd contents offset) -- TODO add better error handling

ffsSetFileSize :: FilePath -> FileOffset -> FilesM Errno
ffsSetFileSize  path offset = do
    debugS "setFileSize" path
    realFilePath <- getRealFilePathFromTagPath path
    (liftIO $ setFileSize realFilePath offset) $> eOK

ffsFlush :: FilePath -> FfsFd -> FilesM Errno
ffsFlush path fd = debugS "flushing" path >> return eOK -- TODO do something

ffsRelease :: FilePath -> FfsFd -> FilesM ()
ffsRelease path fd = debugS "releasing" path >> return ()

ffsSynchronizeFile :: FilePath -> SyncType -> FilesM Errno
ffsSynchronizeFile path _ = debugS "synchronizing" path >> return eOK -- TODO do something

ffsCreateDirectory :: FilePath -> FileMode -> FilesM Errno
ffsCreateDirectory path _ = do
    let tags = getTagsFromPath path
    let tag = newTag (path ^. filename) tags
    debugS "creating directory" tag
    modify (insert tag)
    return eOK


getFileSystemStats :: String -> FilesM (Either Errno FileSystemStats)
getFileSystemStats str =
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
                , TFile TypeTag (Name "/") []
                ]
