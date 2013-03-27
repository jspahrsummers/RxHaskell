{-# LANGUAGE Safe #-}

module Subject ( newSubject
               , newReplaySubject
               , Subject
               , Subscriber
               , send
               , runSignal
               , runSignalIO
               , SubjectCapacity(..)
               ) where

import Control.Concurrent.STM
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Foldable
import Data.Functor
import Data.IORef
import Data.Sequence as Seq
import Data.Traversable
import Disposable
import Prelude hiding (mapM_, length, drop)
import Signal
import Subscriber

-- | A controllable signal, represented by a subscriber (a.k.a. sink) and signal pair.
type Subject m v = (Subscriber m v, SignalM m v)

-- | Determines how many events a replay subject will save.
data SubjectCapacity = LimitedCapacity Int  -- ^ The subject will only save the specified number of events.
                     | UnlimitedCapacity    -- ^ The subject will save an unlimited number of events.
                     deriving (Eq, Show)

-- | Creates a subject.
-- | Sending values on the subscriber will deliver them to all of the signal's current subscribers.
newSubject :: MonadIO m => m (Subject m v)
newSubject = do
    subsRef <- liftIO $ newIORef Seq.empty

    let s =
            signal $ \sub ->
                liftIO $ atomicModifyIORef subsRef $ \seq -> (seq |> sub, EmptyDisposable)

        onEvent ev = do
            subs <- liftIO $ readIORef subsRef
            mapM_ (`send` ev) subs

    sub <- subscriber onEvent
    return (sub, s)

-- | Like 'newSubject', but new subscriptions to the returned signal will receive all values
-- | (up to the specified capacity) which have been sent thus far.
newReplaySubject :: MonadIO m => SubjectCapacity -> m (Subject m v)
newReplaySubject cap = do
    subsVar <- liftIO $ atomically $ newTVar Seq.empty
    eventsVar <- liftIO $ atomically $ newTVar Seq.empty

    let addSubscriber sub = do
            modifyTVar' subsVar (|> sub)
            readTVar eventsVar
        
        s =
            signal $ \sub -> do
                events <- liftIO $ atomically $ addSubscriber sub
                mapM_ (send sub) events
                return EmptyDisposable

        limit n seq =
            if length seq > n
            then drop (length seq - n) seq
            else seq

        addEvent ev =
            let addEvent' (LimitedCapacity c) seq = limit c $ seq |> ev
                addEvent' UnlimitedCapacity seq = seq |> ev
            in do
                modifyTVar' eventsVar (addEvent' cap)
                readTVar subsVar

        onEvent ev = do
            subs <- liftIO $ atomically $ addEvent ev
            mapM_ (`send` ev) subs

    sub <- subscriber onEvent
    return (sub, s)

-- | Subscribes to a signal, executing all of its side effects within monad transformer @t@,
-- | and projecting its events one level down.
runSignal :: (MonadIO m, MonadTrans t, MonadIO (t m)) => SignalM (t m) v -> t m (SignalM m v)
runSignal smt = do
    (sub, sm) <- lift $ newReplaySubject UnlimitedCapacity

    let onEvent ev = lift $ send sub ev

    smt >>: onEvent
    return sm

-- | Subscribes to a signal, executing all of its side effects within monad @m@,
-- | and projecting its events out into the IO monad.
runSignalIO :: MonadIO m => SignalM m v -> m (Signal v)
runSignalIO sm = do
    (sub, si) <- liftIO $ newReplaySubject UnlimitedCapacity

    let onEvent ev = liftIO $ send sub ev

    sm >>: onEvent
    return si
