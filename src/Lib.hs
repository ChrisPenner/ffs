module Lib where

import System.Fuse
import Control.Exception
import Data.IORef
import Foreign.C.Error
import qualified Data.Map as M
import qualified Data.ByteString as BS

newtype Files = Files {unFiles :: IORef (M.Map FilePath BS.ByteString)}

newFiles :: IO Files
newFiles = Files <$> newIORef M.empty

ffs :: Files -> FuseOperations fh
ffs files = defaultFuseOps
    { fuseInit = putStrLn "Welcome to FFS"
    , fuseGetFileStat = \_ -> putStrLn "GetFileStat" >> return (Left eNOSYS)
    , fuseReadSymbolicLink = \_ -> putStrLn "ReadSymbolicLink" >> return (Left eNOSYS)
    , fuseCreateDevice = \_ _ _ _ ->  putStrLn "CreateDevice" >> return eNOSYS
    , fuseCreateDirectory = \_ _ -> putStrLn "CreateDirectory" >> return eNOSYS
    , fuseRemoveLink = \_ -> putStrLn "RemoveLink" >> return eNOSYS
    , fuseRemoveDirectory = \_ -> putStrLn "RemoveDir" >> return eNOSYS
    , fuseCreateSymbolicLink = \_ _ -> putStrLn "CreateSymlink" >> return eNOSYS
    , fuseRename = \_ _ -> putStrLn "Rename" >> return eNOSYS
    , fuseCreateLink = \_ _ -> putStrLn "CreateLink" >> return eNOSYS
    , fuseSetFileMode = \_ _ -> putStrLn "SetFileMode" >> return eNOSYS
    , fuseSetOwnerAndGroup = \_ _ _ -> putStrLn "SetOwnerAndGroup" >> return eNOSYS
    , fuseSetFileSize = \_ _ -> putStrLn "SetFileSize" >> return eNOSYS
    , fuseSetFileTimes = \_ _ _ -> putStrLn "SetFileTimes" >> return eNOSYS
    , fuseOpen =   \_ _ _   -> putStrLn "Open" >> return (Left eNOSYS)
    , fuseRead =   \_ _ _ _ -> putStrLn "Read" >> return (Left eNOSYS)
    , fuseWrite =  \_ _ _ _ -> putStrLn "Write" >> return (Left eNOSYS)
    , fuseGetFileSystemStats = \_ -> putStrLn "GetFileSystemStats" >> return (Left eNOSYS)
    , fuseFlush = \_ _ -> putStrLn "Flush" >> return eOK
    , fuseRelease = \_ _ -> putStrLn "Release" >> return ()
    , fuseSynchronizeFile = \_ _ -> putStrLn "SynchronizeFile" >> return eNOSYS
    , fuseOpenDirectory = \_ -> putStrLn "OpenDir" >> return eNOSYS
    , fuseReadDirectory = \_ -> putStrLn "ReadDir" >> return (Left eNOSYS)
    , fuseReleaseDirectory = \_ -> putStrLn "ReleaseDir" >> return eNOSYS
    , fuseSynchronizeDirectory = \_ _ -> putStrLn "SynchDir" >> return eNOSYS
    , fuseAccess = \_ _ -> putStrLn "Access" >> return eNOSYS
    , fuseDestroy = putStrLn "Goodbye!"
    }

runFFSffs :: IO ()
runFFSffs = do
    files <- newFiles
    fuseMain (ffs files) defaultExceptionHandler
