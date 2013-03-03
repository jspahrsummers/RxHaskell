{-# LANGUAGE Safe #-}

module Subject ( newSubject
               ) where

import Data.Foldable
import Data.Functor
import Data.IORef
import Data.Sequence as Seq
import Data.Traversable
import Disposable
import Event
import Prelude hiding (mapM_)
import Signal
import Subscriber

-- | Creates a controllable signal, represented by a subscriber (a.k.a. sink) and signal pair.
-- | Sending values on the subscriber will deliver them to all of the signal's subscribers.
newSubject :: IO (Subscriber a, Signal a)
newSubject = do
    subj <- newIORef Seq.empty

    let s =
            signal $ \sub ->
                atomicModifyIORef subj $ \seq -> (seq |> sub, Disposable.empty)

        onEvent ev = do
            subs <- readIORef subj
            mapM_ (`send` ev) subs

    sub <- subscriber onEvent
    return (sub, s)
