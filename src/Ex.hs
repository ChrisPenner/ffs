{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ex where

import Prelude hiding (log)
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

import System.Fuse

type HT = ()

type TagPath = String
data FType = Tag | File
    deriving (Show, Eq, Ord)

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

newtype Files = Files {unFiles :: (M.Map String (S.Set (FilePath, FType)))}
makeLensesFor [("unFiles", "files")] ''Files

newtype FilesM a = FilesM {runFilesM :: StateT Files IO a }
    deriving newtype (Functor, Applicative, Monad, MonadState Files, MonadIO)

transformOps :: BetterFuseOps fh FilesM -> IO (FuseOperations fh)
transformOps BetterFuseOps{..} = do
    filesRef <- newFiles
    exampleTags filesRef
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

exampleTags :: IORef Files -> IO ()
exampleTags ref = do
    modifyIORef ref
                (files
                 <>~ M.fromList [ ( "photos"
                                  , S.fromList [("pic.jpg", File), ("image.png", File), ("sandy-beach.png", File)]
                                  )
                                , ("movies", S.fromList [("anime.mkv", File)])
                                , ("vacation"
                                  , S.fromList [("location", Tag), ("sandy-beach.png", File)]
                                  )
                                ])

wrapState :: IORef Files -> FilesM a -> IO a
wrapState ref m = do
    files <- readIORef ref
    (a, files') <- flip runStateT files $ runFilesM m
    writeIORef ref files'
    return a

newFiles :: IO (IORef Files)
newFiles = newIORef $ Files M.empty

main :: IO ()
main = do
    fsOps <- transformOps helloFSOps
    fuseMain fsOps (\e -> logS e >> defaultExceptionHandler e)

helloFSOps :: BetterFuseOps HT FilesM
helloFSOps = BetterFuseOps { mfuseGetFileStat = helloGetFileStat
                           , mfuseOpen        = helloOpen
                           , mfuseRead        = helloRead
                           , mfuseOpenDirectory = openTag
                           , mfuseReadDirectory = taggedWith
                           , mfuseGetFileSystemStats = helloGetFileSystemStats
                           , mfuseInit = helloInit
                           }


helloInit :: FilesM ()
helloInit = log "Init!"

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

helloGetFileStat :: FilePath -> FilesM (Either Errno FileStat)
helloGetFileStat _ = do
    ctx <- liftIO getFuseContext
    return $ Right $ dirStat ctx
-- helloGetFileStat path | path == helloPath = do
--     ctx <- liftIO getFuseContext
--     return $ Right $ fileStat ctx
-- helloGetFileStat _ =
--     return $ Left eNOENT

openTag :: TagPath -> FilesM Errno
openTag "/" = return eOK
openTag tagPath = do
    let tag = tagPath ^. basename
    exists <- uses files (has (ix tag))
    if exists then return eOK
              else return eNOENT

taggedWith :: TagPath -> FilesM (Either Errno [(FilePath, FileStat)])
taggedWith "/" = do
    log ("reading: " <> "/")
    ctx <- liftIO $ getFuseContext
    tagFiles <- gets (M.keys . unFiles)
    log ("found tagged: " <> show tagFiles)
    pure . Right
        $ [(".", dirStat ctx), ("..", dirStat ctx)] <> ((, fileStat ctx) <$> tagFiles)

taggedWith tagPath = do
    let tags = tail $ splitDirectories tagPath -- Strip of leading "/"
    log ("reading: " <> show tags)
    ctx <- liftIO $ getFuseContext
    tagMap <- use files
    let allTaggedFiles :: [S.Set (FilePath, FType)]
          = fmap (\tag -> fromMaybe mempty (M.lookup tag tagMap)) tags
    let hasAllTags :: S.Set (FilePath, FType)
          = foldr' S.intersection (fold allTaggedFiles) $ allTaggedFiles
    log ("found tagged: " <> show hasAllTags)
    pure . Right
        $ [(".", dirStat ctx), ("..", dirStat ctx)] <> (second (toStat ctx) <$> S.toList hasAllTags)

toStat ::  FuseContext -> FType -> FileStat
toStat ctx Tag = dirStat ctx
toStat ctx File = fileStat ctx

helloOpen :: FilePath -> OpenMode -> OpenFileFlags -> FilesM (Either Errno HT)
helloOpen path mode flags
    | path == helloPath = case mode of
                            ReadOnly -> return (Right ())
                            _        -> return (Left eACCES)
    | otherwise         = return (Left eNOENT)


helloRead :: FilePath -> HT -> ByteCount -> FileOffset -> FilesM (Either Errno B.ByteString)
helloRead path _ byteCount offset
    | path == helloPath =
        return $ Right $ B.take (fromIntegral byteCount) $ B.drop (fromIntegral offset) helloString
    | otherwise         = return $ Left eNOENT

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

log :: MonadIO m => String -> m ()
log s = do
    liftIO $ appendFile "/Users/chris/fuse.log" (s <> "\n")


logS :: (Show a, MonadIO m) => a -> m ()
logS s = do
    liftIO $ appendFile "/Users/chris/fuse.log" (show s <> "\n")
