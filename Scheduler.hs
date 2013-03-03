{-# LANGUAGE Safe #-}

module Scheduler ( Scheduler
                 , newScheduler
                 , schedule
                 ) where

import Control.Concurrent
import Control.Concurrent.STM
import Data.IORef
import Disposable

-- | Represents an action on a scheduler, along with a flag indicating whether it should be canceled.
type ScheduledAction = (IORef Bool, IO ())

-- | Executes actions serially in FIFO order.
data Scheduler = Scheduler {
    thread :: ThreadId,
    queue :: TQueue ScheduledAction
}

-- | Creates a new background scheduler.
newScheduler :: IO Scheduler
newScheduler = do
    q <- atomically $ newTQueue
    tid <- forkIO $ schedulerMain q

    return $ Scheduler {
        thread = tid,
        queue = q
    }

-- | Schedules @action@ on @s@. Returns a disposable which can be used to cancel it.
schedule :: Scheduler -> IO () -> IO Disposable
schedule s action = do
    ref <- newIORef False
    atomically $ writeTQueue (queue s) (ref, action)

    newDisposable $ atomicModifyIORef ref $ const (True, ())

schedulerMain :: TQueue ScheduledAction -> IO ()
schedulerMain q = do
    (ref, action) <- atomically $ readTQueue q

    d <- readIORef ref
    if d then return () else action

    yield
    schedulerMain q
