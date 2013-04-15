{-# LANGUAGE Safe #-}

module Signal.Subscriber.Internal ( Subscriber(..)
                                  , addSubscriptionDisposable
                                  , disposeSubscriber
                                  ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Data.Word
import Disposable
import Scheduler
import Signal.Event

-- | Receives events from a signal with values of type @v@ and running in a scheduler of type @s@.
--
--   Note that @s@ refers to the scheduler that events must be sent on. Events are always sent
--   synchronously, regardless of @s@.
data Subscriber s v = Subscriber {
    onEvent :: Event v -> SchedulerIO s (),
    disposables :: DisposableSet,
    lockedThread :: TVar ThreadId,
    threadLockCounter :: TVar Word32,
    disposed :: TVar Bool
}

-- | Adds a disposable representing a subscription to the subscriber.
--   If the subscriber is later sent completed or error, the disposable will be disposed.
addSubscriptionDisposable :: Subscriber s v -> Disposable -> IO ()
addSubscriptionDisposable sub d = addDisposable (disposables sub) d

-- | Disposes the subscriber, preventing it from receiving any new events.
disposeSubscriber :: Subscriber s v -> IO ()
disposeSubscriber s = toDisposable (disposables s) >>= dispose
