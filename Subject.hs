{-# LANGUAGE Safe #-}

module Subject ( newSubject
               , Subscriber
               , send
               , runSignal
               , runSignalIO
               ) where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Foldable
import Data.Functor
import Data.IORef
import Data.Sequence as Seq
import Data.Traversable
import Disposable
import Prelude hiding (mapM_)
import Signal
import Subscriber

-- | Creates a controllable signal, represented by a subscriber (a.k.a. sink) and signal pair.
-- | Sending values on the subscriber will deliver them to all of the signal's subscribers.
newSubject :: MonadIO m => m (Subscriber m v, SignalM m v)
newSubject = do
    subj <- liftIO $ newIORef Seq.empty

    let s =
            signal $ \sub ->
                liftIO $ atomicModifyIORef subj $ \seq -> (seq |> sub, EmptyDisposable)

        onEvent ev = do
            subs <- liftIO $ readIORef subj
            mapM_ (`send` ev) subs

    sub <- subscriber onEvent
    return (sub, s)

-- | Subscribes to a signal, executing all of its side effects within monad transformer @t@,
-- | and projecting its events one level down.
runSignal :: (MonadIO m, MonadTrans t, MonadIO (t m)) => SignalM (t m) v -> t m (SignalM m v)
runSignal smt = do
    (sub, sm) <- lift newSubject

    let onEvent ev = lift $ send sub ev

    smt >>: onEvent
    return sm

-- | Subscribes to a signal, executing all of its side effects within monad @m@,
-- | and projecting its events out into the IO monad.
runSignalIO :: MonadIO m => SignalM m v -> m (Signal v)
runSignalIO sm = do
    (sub, si) <- liftIO newSubject

    let onEvent ev = liftIO $ send sub ev

    sm >>: onEvent
    return si
