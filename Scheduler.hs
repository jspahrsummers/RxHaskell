{-# LANGUAGE Safe #-}

module Scheduler ( Scheduler
                 , newScheduler
                 , schedule
                 ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
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
    q <- atomically newTQueue

    -- TODO: Spawn scheduler threads lazily (when the first action is enqueued).
    tid <- forkIO $ schedulerMain q

    return Scheduler {
        thread = tid,
        queue = q
    }

-- | Schedules @action@ on @s@. Returns a disposable which can be used to cancel it.
schedule :: Scheduler -> IO () -> IO (Disposable IO)
schedule s action = do
    ref <- newIORef False
    atomically $ writeTQueue (queue s) (ref, action)

    newDisposable $ atomicModifyIORef ref $ const (True, ())

-- | Executes all current and future actions enqueued on the given scheduler.
schedulerMain :: TQueue ScheduledAction -> IO ()
schedulerMain q = do
    -- TODO: If the queue is empty, we should kill this scheduler's thread until a new action is enqueued.
    (ref, action) <- atomically $ readTQueue q

    d <- readIORef ref
    unless d action

    yield
    schedulerMain q
