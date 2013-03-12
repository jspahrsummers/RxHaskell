{-# LANGUAGE Safe #-}

module Signal.Scheduled ( start
                        , Scheduler
                        , newScheduler
                        , subscribeOn
                        ) where

import Control.Monad
import Control.Monad.IO.Class
import Disposable
import Scheduler
import Signal
import Subject

-- | Starts a signal which executes @action@ on @s@.
start :: Scheduler -> (Subscriber IO v -> IO ()) -> IO (Signal v)
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

-- | Returns a signal which delivers the events of @sig@ on scheduler @sch@.
deliverOn :: Signal v -> Scheduler -> Signal v
deliverOn sig sch =
    signal $ \sub -> do
        -- Although we could hold onto any disposable returned from scheduling,
        -- the complexity of managing all of them probably isn't worth the
        -- slightly faster cancellation.
        let onEvent ev = void $ schedule sch $ send sub ev

        sig >>: onEvent
