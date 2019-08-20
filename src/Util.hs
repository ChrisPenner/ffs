module Util where

import Control.Monad.IO.Class

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

