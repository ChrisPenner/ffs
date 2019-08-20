{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Control.Monad.IO.Class
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

import Polysemy
import Polysemy.State
import Polysemy.Input
import Polysemy.Output
import Control.Monad

import Persist
import Types
import Ops
import Util

import System.Fuse

type FfsFd = Fd

type TagPath = String

-- data BetterFuseOps fh m = BetterFuseOps  {
--         mfuseGetFileStat :: FilePath -> m (Either Errno FileStat),
--         -- mfuseReadSymbolicLink :: FilePath -> m (Either Errno FilePath),
--         mfuseCreateDevice :: FilePath -> EntryType -> FileMode -> DeviceID -> m Errno,
--         mfuseCreateDirectory :: FilePath -> FileMode -> m Errno,
--         -- mfuseRemoveLink :: FilePath -> m Errno,
--         -- mfuseRemoveDirectory :: FilePath -> m Errno,
--         -- mfuseCreateSymbolicLink :: FilePath -> FilePath -> m Errno,
--         -- mfuseRename :: FilePath -> FilePath -> m Errno,
--         -- mfuseCreateLink :: FilePath -> FilePath -> m Errno,
--         -- mfuseSetFileMode :: FilePath -> FileMode -> m Errno,
--         -- mfuseSetOwnerAndGroup :: FilePath -> UserID -> GroupID -> m Errno,
--         mfuseSetFileSize :: FilePath -> FileOffset -> m Errno,
--         -- mfuseSetFileTimes :: FilePath -> EpochTime -> EpochTime -> m Errno,
--         mfuseOpen :: FilePath -> OpenMode -> OpenFileFlags -> m (Either Errno fh),
--         mfuseRead :: FilePath -> fh -> ByteCount -> FileOffset
--                  -> m (Either Errno B.ByteString),
--         mfuseWrite :: FilePath -> fh -> B.ByteString -> FileOffset
--                   -> m (Either Errno ByteCount),
--         mfuseGetFileSystemStats :: String -> m (Either Errno FileSystemStats),
--         mfuseFlush :: FilePath -> fh -> Sem r Errno,
--         mfuseRelease :: FilePath -> fh -> Sem r (),
--         mfuseSynchronizeFile :: FilePath -> SyncType -> Sem r Errno,
--         mfuseOpenDirectory :: FilePath -> m Errno,
--         mfuseReadDirectory :: FilePath -> m (Either Errno [(FilePath, FileStat)]),
--         -- mfuseReleaseDirectory :: FilePath -> Sem r Errno,
--         -- mfuseSynchronizeDirectory :: FilePath -> SyncType -> Sem r Errno,
--         -- mfuseAccess :: FilePath -> Int -> Sem r Errno,
--         mfuseInit :: m (),
--         mfuseDestroy :: m ()
--       }

main :: IO ()
main = do
    prog <- getProgName
    (rootFS, args) <- getArgs >>= \case
        (rootFS:args) -> do
            exists <- doesDirectoryExist rootFS
            when (not exists) $ hPutStrLn stderr ("Bad file root: " <> rootFS) >> exitFailure
            return (rootFS, args)
        args -> hPutStrLn stderr "usage: ffs <files> <mount point>" >> exitFailure

    persistDir <- getPersistenceDir
    let env = Env rootFS persistDir
    tagMap <- loadIndex
    runM . runOutputSem debug . runInputConst env . evalState tagMap $ withLowerToIO $
            \lowerSem _finalize -> do
                fuseRun lowerSem prog args ffsOps (\e -> debugS "Error" e >> defaultExceptionHandler e)

ffsOps :: (Member (Output String) r, Member (Embed IO) r, Member (State TagMap) r, Member (Input Env) r)
       => FuseOperations FfsFd (Sem r)
ffsOps = FuseOperations { fuseGetFileStat = ffsGetFileStat
                        , fuseOpen        = ffsOpen
                        , fuseRead        = ffsReadFile
                        , fuseOpenDirectory = ffsOpenDirectory
                        , fuseReadDirectory = ffsReadDirectory
                        , fuseGetFileSystemStats = getFileSystemStats
                        , fuseInit = initFFS
                        , fuseDestroy = destroyFFS
                        , fuseWrite = ffsWriteFile
                        , fuseSetFileSize = ffsSetFileSize
                        , fuseFlush = ffsFlush
                        , fuseRelease = ffsRelease
                        , fuseSynchronizeFile = ffsSynchronizeFile
                        , fuseCreateDirectory = ffsCreateDirectory
                        , fuseCreateDevice = ffsCreateDevice

                        , fuseReadSymbolicLink = \_ -> debug "symbolicLink" >> error "symbolicLink"
                        , fuseRemoveLink = \_ -> debug "fuseRemoveLink" >> error "fuseRemoveLink"
                        , fuseRemoveDirectory = \_ -> debug "fuseRemoveDirectory" >> error "fuseRemoveDirectory"
                        , fuseCreateSymbolicLink = \_ _ -> debug "fuseCreateSymbolicLink" >> error "fuseCreateSymbolicLink"
                        , fuseRename = \_ _ -> debug "fuseRename" >> error "fuseRename"
                        , fuseCreateLink = \_ _ -> debug "fuseCreateLink" >> error "fuseCreateLink"
                        , fuseSetFileMode = \_ _ -> debug "fuseSetFileMode" >> error "fuseSetFileMode"
                        , fuseSetOwnerAndGroup = \_ _ _ -> debug "fuseSetOwnerAndGroup" >> error "fuseSetOwnerAndGroup"
                        , fuseReleaseDirectory = \_ -> debug "fuseReleaseDirectory" >> error "fuseReleaseDirectory"
                        , fuseSynchronizeDirectory = \_ _ -> debug "fuseSynchronizeDirectory" >> error "fuseSynchronizeDirectory"
                        , fuseAccess = \_ _ -> debug "fuseAccess" >> error "fuseAccess"
                        , fuseSetFileTimes = \_ _ _ -> debug "fuseSetFileTimes" >> error "fuseSetFileTimes"
                        }

-- Fix this dumb assumption
initFFS :: Monad m => m ()
initFFS = return ()

-- Fix this dumb assumption
destroyFFS :: (Member (Embed IO) r, Member (Input Env) r, Member (State TagMap) r) => Sem r ()
destroyFFS = persistIndex

ffsGetFileStat :: (Member (Output String) r, Member (Input Env) r, Member (Embed IO) r, Member (State TagMap) r) => FilePath -> Sem r (Either Errno FileStat)
ffsGetFileStat filePath = do
    debugS "reading file stat" filePath
    ctx <- liftIO getFuseContext
    fileFromPath filePath >>= \case
        Just f@(view fileType -> TypeTag)  -> return . Right $ defaultDirStat ctx
        Just f@(view fileType -> TypeFile) -> Right <$> statFile ctx f
        Nothing -> do
            return $ Left eNOENT
            -- return . Right $ defaultDirStat ctx

ffsOpenDirectory :: (Member (Output String) r, Member (Embed IO) r, Member (State TagMap) r) => TagPath -> Sem r Errno
ffsOpenDirectory tagPath = do
    debugS "opening tag" tagPath
    exists <- isJust <$> fileFromPath tagPath
    if exists
        then debug "Success" >> return eOK
        else debug "Failure" >> return eNOENT

ffsReadDirectory :: (Member (Input Env) r, Member (Embed IO) r, Member (State TagMap) r) => TagPath -> Sem r (Either Errno [(FilePath, FileStat)])
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


ffsOpen :: (Member (Output String) r, Member (Input Env) r, Member (Embed IO) r, Member (State TagMap) r) => FilePath -> OpenMode -> OpenFileFlags -> Sem r (Either Errno Fd)
ffsOpen path mode flags = do
    debugS "opening: " path
    realFilePathM <- fileFromPath path >>= traverse getRealFilePath
    debugS "file found" realFilePathM
    case realFilePathM of
        (Just realFilePath) -> do
            Right <$> (liftIO $ openFd realFilePath mode Nothing flags)
        Nothing -> return (Left eNOENT)

ffsReadFile :: (Member (Embed IO) r) => FilePath -> FfsFd -> ByteCount -> FileOffset -> Sem r (Either Errno B.ByteString)
ffsReadFile  path fd byteCount offset = do
    debugS "readFile " path
    liftIO $ (Right <$> fdPread fd byteCount offset) -- TODO add better error handling

ffsWriteFile :: (Member (Embed IO) r) =>FilePath -> FfsFd -> BS.ByteString -> FileOffset -> Sem r (Either Errno ByteCount)
ffsWriteFile  path fd contents offset = do
    debugS "writeFile " path
    liftIO $ (Right <$> fdPwrite fd contents offset) -- TODO add better error handling

ffsSetFileSize :: (Member (Input Env) r, Member (Embed IO) r) => FilePath -> FileOffset -> Sem r Errno
ffsSetFileSize  path offset = do
    debugS "setFileSize" path
    realFilePath <- getRealFilePathFromTagPath path
    (liftIO $ setFileSize realFilePath offset) $> eOK

ffsFlush :: (Member (Embed IO) r) => FilePath -> FfsFd -> Sem r Errno
ffsFlush path fd = debugS "flushing" path >> return eOK -- TODO do something

ffsRelease :: (Member (Embed IO) r) => FilePath -> FfsFd -> Sem r ()
ffsRelease path fd = debugS "releasing" path >> return ()

ffsSynchronizeFile :: (Member (Embed IO) r) => FilePath -> SyncType -> Sem r Errno
ffsSynchronizeFile path _ = debugS "synchronizing" path >> return eOK -- TODO do something

ffsCreateDirectory :: (Member (State TagMap) r, Member (Embed IO) r) => FilePath -> FileMode -> Sem r Errno
ffsCreateDirectory path _ = do
    let tags = getTagsFromPath path
    let tag = newTag (path ^. filename) tags
    debugS "creating directory" tag
    modify (insert tag)
    return eOK

ffsCreateDevice :: (Member (Input Env) r, Member (Embed IO) r)
                => FilePath
                -> EntryType
                -> FileMode
                -> DeviceID
                -> Sem r Errno
ffsCreateDevice path RegularFile mode id  = do
    realPath <- getRealFilePathFromTagPath path
    debugS "createFile" path
    debugS "Creating" realPath
    liftIO $ createDevice realPath mode id
    debugS "DONE" realPath
    return eOK -- TODO real error handling
ffsCreateDevice path typ _mode _id  = do
    debugS "createDevice unimplemented for type" typ
    return eACCES

getFileSystemStats :: String -> Sem r (Either Errno FileSystemStats)
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


--  Delete this
runOutputSem :: (o -> Sem r ()) -> Sem (Output o ': r) a -> Sem r a
runOutputSem act = interpret $ \case
    Output o -> act o
