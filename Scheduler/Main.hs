{-# LANGUAGE Trustworthy #-}

module Scheduler.Main ( Scheduler
                      , schedule
                      , getMainScheduler
                      , runMainScheduler
                      ) where

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

-- | Runs the main scheduler indefinitely.
runMainScheduler :: IO ()
runMainScheduler = getMainScheduler >>= schedulerMain
