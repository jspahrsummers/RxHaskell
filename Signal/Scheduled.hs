{-# LANGUAGE Safe #-}

module Signal.Scheduled ( start
                        ) where

import Scheduler
import Signal
import Subject
import Subscriber

-- | Starts a signal which executes @action@ on @s@.
start :: Scheduler -> (Subscriber a -> IO ()) -> IO (Signal a)
start s action = do
    (sub, sig) <- newSubject
    schedule s $ action sub
    return sig
