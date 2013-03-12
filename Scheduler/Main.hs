{-# LANGUAGE Trustworthy #-}

module Scheduler.Main ( Scheduler
                      , schedule
                      , getMainScheduler
                      , runMainScheduler
                      ) where

import Control.Concurrent
import Control.Concurrent.STM
import Data.IORef
import Scheduler
import Scheduler.Internal
import System.IO.Unsafe

-- ohai global variable
mainSchedulerRef :: IORef Scheduler
{-# NOINLINE mainSchedulerRef #-}
mainSchedulerRef =
    unsafePerformIO $ do
        q <- atomically newTQueue
        newIORef $ IndefiniteScheduler q

-- | Returns a scheduler representing the main thread.
-- | Note that 'runMainScheduler' must be called for enqueued actions to actually execute.
getMainScheduler :: IO Scheduler
getMainScheduler = readIORef mainSchedulerRef

-- | Runs the main scheduler indefinitely using the current thread.
-- | The current thread will be bound if possible.
runMainScheduler :: IO ()
runMainScheduler =
    let run = getMainScheduler >>= schedulerMain
    in if rtsSupportsBoundThreads
        then runInBoundThread run
        else run
