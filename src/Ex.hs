{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Ex where

import qualified Data.ByteString.Char8 as B
import Foreign.C.Error
import System.Posix.Types
import System.Posix.Files
import System.Posix.IO
import Data.IORef
import Control.Monad.State
import qualified Data.ByteString as BS
import qualified Data.Map as M

import System.Fuse

type HT = ()



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
        mfuseReadDirectory :: FilePath -> m (Either Errno [(FilePath, FileStat)])
        -- fuseReleaseDirectory :: FilePath -> IO Errno,
        -- fuseSynchronizeDirectory :: FilePath -> SyncType -> IO Errno,
        -- fuseAccess :: FilePath -> Int -> IO Errno,
        -- fuseInit :: IO (),
        -- fuseDestroy :: IO ()
      }

newtype Files = Files {unFiles :: (M.Map FilePath BS.ByteString)}
newtype FilesM a = FilesM {runFilesM :: StateT Files IO a }
    deriving newtype (Functor, Applicative, Monad, MonadState Files, MonadIO)

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

wrapState :: IORef Files -> FilesM a -> IO a
wrapState ref m = do
    files <- readIORef ref
    (a, files') <- flip runStateT files $ runFilesM m
    writeIORef ref files'
    return a

newFiles :: IO (IORef Files)
newFiles = newIORef $ Files M.empty

main :: IO ()
main = fuseMain helloFSOps defaultExceptionHandler

helloFSOps :: FuseOperations HT
helloFSOps = defaultFuseOps { fuseGetFileStat = helloGetFileStat
                            , fuseOpen        = helloOpen
                            , fuseRead        = helloRead
                            , fuseOpenDirectory = helloOpenDirectory
                            , fuseReadDirectory = helloReadDirectory
                            , fuseGetFileSystemStats = helloGetFileSystemStats
                            }


helloString :: B.ByteString
helloString = B.pack "Hello World, HFuse!\n"

helloPath :: FilePath
helloPath = "/hello"

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
                        , statFileSize = fromIntegral $ B.length helloString
                        , statBlocks = 1
                        , statAccessTime = 0
                        , statModificationTime = 0
                        , statStatusChangeTime = 0
                        }

helloGetFileStat :: FilePath -> IO (Either Errno FileStat)
helloGetFileStat "/" = do
    ctx <- getFuseContext
    return $ Right $ dirStat ctx
helloGetFileStat path | path == helloPath = do
    ctx <- getFuseContext
    return $ Right $ fileStat ctx
helloGetFileStat _ =
    return $ Left eNOENT

helloOpenDirectory :: Monad m => [Char] -> m Errno
helloOpenDirectory "/" = return eOK
helloOpenDirectory _   = return eNOENT

helloReadDirectory :: FilePath -> IO (Either Errno [(FilePath, FileStat)])
helloReadDirectory "/" = do

    ctx <- getFuseContext
    return $ Right [(".",          dirStat  ctx)
                   ,("..",         dirStat  ctx)
                   ,(helloName,    fileStat ctx)
                   ]
    where (_:helloName) = helloPath
helloReadDirectory _ = return (Left (eNOENT))

helloOpen :: FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno HT)
helloOpen path mode flags
    | path == helloPath = case mode of
                            ReadOnly -> return (Right ())
                            _        -> return (Left eACCES)
    | otherwise         = return (Left eNOENT)


helloRead :: FilePath -> HT -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
helloRead path _ byteCount offset
    | path == helloPath =
        return $ Right $ B.take (fromIntegral byteCount) $ B.drop (fromIntegral offset) helloString
    | otherwise         = return $ Left eNOENT

helloGetFileSystemStats :: String -> IO (Either Errno FileSystemStats)
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
