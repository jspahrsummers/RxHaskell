{-# LANGUAGE Safe #-}

module Signal.Scheduled ( start
                        , Scheduler
                        , newScheduler
                        , subscribeOn
                        ) where

import Disposable
import Scheduler
import Signal
import Subject

-- | Starts a signal which executes @action@ on @s@.
start :: Scheduler -> (Subscriber IO a -> IO ()) -> IO (Signal a)
start s action = do
    (sub, sig) <- newReplaySubject
    schedule s $ action sub
    return sig

-- | Returns a signal which subscribes to @sig@ on scheduler @sch@.
subscribeOn :: Signal v -> Scheduler -> Signal v
subscribeOn sig sch =
    signal $ \sub -> do
        ds <- newDisposableSet
        schD <- schedule sch (sig `subscribe` sub >>= addDisposable ds)

        addDisposable ds schD
        toDisposable ds
