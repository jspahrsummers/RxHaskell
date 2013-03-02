{-# LANGUAGE Safe #-}

module Subject (subject) where

import Control.Concurrent.MVar
import Data.Foldable
import Data.Functor
import Data.Sequence as Seq
import Prelude hiding (sequence_)
import Signal

-- | Creates a controllable signal, represented by a subscriber (a.k.a. sink) and signal pair.
-- | Sending values on the subscriber will deliver them to all of the signal's subscribers.
subject :: IO (Subscriber a, Signal a)
subject = do
    subj <- newMVar Seq.empty

    let s = signal $ \sub ->
                modifyMVar_ subj $ return . flip (|>) sub
        sub m = readMVar subj >>= sequence_ . fmap (\sub -> sub m)

    return (sub, s)
