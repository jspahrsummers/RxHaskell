{-# LANGUAGE Safe #-}

module Subscriber ( Subscriber
                  , subscriber
                  , send
                  , Event(..)
                  ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Data.Word
import Disposable
import Event

-- | Receives events from a signal.
data Subscriber m v = Subscriber {
    onEvent :: Event v -> m (),
    disposable :: Disposable m,
    lockedThread :: TVar ThreadId,
    threadLockCounter :: TVar Word32,
    disposed :: TVar Bool
}

-- | Constructs a subscriber.
subscriber :: MonadIO m => (Event v -> m ()) -> m (Subscriber m v)
subscriber f = do
    b <- liftIO $ atomically $ newTVar False
    d <- newDisposable $ liftIO $ atomically $ writeTVar b True

    tid <- liftIO myThreadId
    lt <- liftIO $ atomically $ newTVar tid
    tlc <- liftIO $ atomically $ newTVar 0

    return Subscriber {
        onEvent = f,
        disposable = d,
        lockedThread = lt,
        threadLockCounter = tlc,
        disposed = b
    }

-- | Acquires a subscriber for the specified thread.
-- | This is used to ensure that a subscriber never receives multiple events concurrently.
acquireSubscriber :: Subscriber m v -> ThreadId -> STM Bool
acquireSubscriber sub tid = do
    d <- readTVar (disposed sub)
    if d
        then return False
        else do
            tlc <- readTVar (threadLockCounter sub)
            lt <- readTVar (lockedThread sub)
            when (tlc > 0 && lt /= tid) retry

            writeTVar (lockedThread sub) tid
            writeTVar (threadLockCounter sub) $ tlc + 1
            return True

-- | Releases a subscriber from the specified thread's ownership.
releaseSubscriber :: Subscriber m v -> ThreadId -> STM ()
releaseSubscriber sub tid = do
    always $ fmap (== tid) $ readTVar (lockedThread sub)

    tlc <- readTVar (threadLockCounter sub)
    always $ return $ tlc > 0

    writeTVar (threadLockCounter sub) $ tlc - 1

-- | Sends an event to a subscriber.
send :: MonadIO m => Subscriber m v -> Event v -> m ()
send s ev =
    let send' ev@(NextEvent _) = onEvent s ev
        send' ev = do
            d <- liftIO $ atomically $ readTVar (disposed s)
            unless d $ onEvent s ev
    in do
        tid <- liftIO myThreadId
        b <- liftIO $ atomically $ acquireSubscriber s tid

        when b $ send' ev >> liftIO (atomically (releaseSubscriber s tid))
