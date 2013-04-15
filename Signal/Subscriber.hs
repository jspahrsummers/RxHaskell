{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Signal.Subscriber ( Subscriber
                         , subscriber
                         , send
                         , Event(..)
                         ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Disposable
import Scheduler
import Signal.Event
import Signal.Subscriber.Internal

-- | Constructs a subscriber.
subscriber
    :: Scheduler s
    => (Event v -> SchedulerIO s ())    -- ^ An action to run when each event is received.
    -> IO (Subscriber s v)              -- ^ The constructed subscriber.

subscriber f = do
    b <- atomically $ newTVar False
    d <- newDisposable $ atomically $ writeTVar b True

    ds <- newDisposableSet
    addDisposable ds d

    tid <- myThreadId
    lt <- atomically $ newTVar tid
    tlc <- atomically $ newTVar 0

    return Subscriber {
        onEvent = f,
        disposables = ds,
        lockedThread = lt,
        threadLockCounter = tlc,
        disposed = b
    }

-- | Acquires a subscriber for the specified thread.
--
--   This is used to ensure that a subscriber never receives multiple events concurrently.
acquireSubscriber :: Subscriber s v -> ThreadId -> STM Bool
acquireSubscriber sub tid = do
    d <- readTVar (disposed sub)
    if d
        then return False
        else do
            -- TODO: Skip all this synchronization for singleton scheduler types.
            tlc <- readTVar (threadLockCounter sub)
            lt <- readTVar (lockedThread sub)
            when (tlc > 0 && lt /= tid) retry

            writeTVar (lockedThread sub) tid
            writeTVar (threadLockCounter sub) $ tlc + 1
            return True

-- | Releases a subscriber from the specified thread's ownership.
--
--   This must match a previous call to 'acquireSubscriber'.
releaseSubscriber :: Subscriber s v -> ThreadId -> STM ()
releaseSubscriber sub tid = do
    -- TODO: Skip all this synchronization for singleton scheduler types.
    ltid <- readTVar $ lockedThread sub
    unless (ltid == tid) $ throwSTM $ userError $ "Locked thread " ++ show ltid ++ " does not match current thread " ++ show tid

    tlc <- readTVar (threadLockCounter sub)
    unless (tlc > 0) $ throwSTM $ userError "Thread lock count is not greater than zero"

    writeTVar (threadLockCounter sub) $ tlc - 1

-- | Synchronously sends an event to a subscriber.
send :: forall s v. Scheduler s => Subscriber s v -> Event v -> SchedulerIO s ()
send s ev =
    let sendAndDispose :: Event v -> SchedulerIO s ()
        sendAndDispose ev = do
            liftIO $ disposeSubscriber s
            onEvent s ev
        
        send' :: Event v -> SchedulerIO s ()
        send' ev@(NextEvent _) = onEvent s ev
        send' ev = do
            wasDisposed <- liftIO $ atomically $ swapTVar (disposed s) True
            unless wasDisposed $ sendAndDispose ev
    in do
        tid <- liftIO myThreadId
        b <- liftIO $ atomically $ acquireSubscriber s tid

        when b $ send' ev >> liftIO (atomically (releaseSubscriber s tid))
