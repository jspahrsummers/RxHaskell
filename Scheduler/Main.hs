{-# LANGUAGE Trustworthy #-}

module Scheduler.Main ( MainScheduler
                      , getMainScheduler
                      , runMainScheduler
                      ) where

import Control.Concurrent
import Control.Concurrent.STM
import Data.IORef
import Scheduler
import Scheduler.Internal
import System.IO.Unsafe

-- | A scheduler which runs enqueued actions on the main thread.
newtype MainScheduler = MainScheduler (TQueue (ScheduledAction MainScheduler))

instance Scheduler MainScheduler where
    schedule (MainScheduler q) action = do
        (sa, d) <- newScheduledAction action
        atomically $ writeTQueue q sa
        return d

    schedulerMain s@(MainScheduler q) = do
        sa <- atomically $ readTQueue q
        executeScheduledAction s sa

-- ohai global variable
mainSchedulerRef :: IORef MainScheduler
{-# NOINLINE mainSchedulerRef #-}
mainSchedulerRef =
    unsafePerformIO $ do
        q <- atomically newTQueue
        newIORef $ MainScheduler q

-- | Returns a scheduler representing the main thread.
--
--   Note that 'runMainScheduler' must be called for enqueued actions to actually execute.
getMainScheduler :: IO MainScheduler
getMainScheduler = readIORef mainSchedulerRef

-- | Runs the main scheduler indefinitely using the current thread.
--   The current thread will be bound if possible.
runMainScheduler :: IO ()
runMainScheduler =
    let run = getMainScheduler >>= schedulerMain
    in if rtsSupportsBoundThreads
        then runInBoundThread run
        else run
