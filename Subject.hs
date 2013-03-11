{-# LANGUAGE Safe #-}

module Subject ( newSubject
               , Subscriber
               , send
               ) where

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
