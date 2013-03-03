{-# LANGUAGE Safe #-}

module Subject (subject) where

import Data.Foldable
import Data.Functor
import Data.IORef
import Data.Sequence as Seq
import Prelude hiding (sequence_)
import Signal

-- | Creates a controllable signal, represented by a subscriber (a.k.a. sink) and signal pair.
-- | Sending values on the subscriber will deliver them to all of the signal's subscribers.
subject :: IO (Subscriber a, Signal a)
subject = do
    subj <- newIORef Seq.empty

    let s = signal $ \sub ->
                atomicModifyIORef subj $ \seq -> (seq |> sub, ())
        sub m = readIORef subj >>= sequence_ . fmap (\sub -> sub m)

    return (sub, s)
