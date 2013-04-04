{-# LANGUAGE Safe #-}

module Signal.Operators ( fromFoldable
                        , materialize
                        , dematerialize
                        , filter
                        , doEvent
                        , doNext
                        , doCompleted
                        , take
                        , drop
                        , switch
                        , combine
                        , first
                        , never
                        , Signal.empty
                        , Signal
                        ) where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.IORef
import Data.Monoid
import Prelude hiding (filter, take, drop)
import Disposable
import Scheduler
import Signal
import Subscriber

-- | Turns any Foldable into a signal.
fromFoldable :: Foldable t => t v -> Signal ImmediateScheduler v
fromFoldable = foldMap return

-- | Treats every signal event as a 'NextEvent' containing the event itself.
-- | This can be used to make all signal events bindable.
materialize :: Scheduler s => Signal s v -> Signal s (Event v)
materialize s =
    signal $ \sub ->
        let onEvent CompletedEvent = send sub (NextEvent CompletedEvent) >> send sub CompletedEvent
            onEvent ev = send sub $ NextEvent ev
        in s >>: onEvent

-- | The inverse of 'materialize'.
dematerialize :: Scheduler s => Signal s (Event v) -> Signal s v
dematerialize s =
    signal $ \sub ->
        let onEvent (NextEvent ev) = send sub ev
            onEvent _ = return ()
        in s >>: onEvent

-- | Filters the values of a signal according to a predicate.
filter :: Scheduler s => Signal s v -> (v -> Bool) -> Signal s v
filter s f =
    let f' x = if f x then return x else mempty
    in s >>= f'

-- | Runs a side-effecting action whenever the signal sends an event.
doEvent :: Scheduler s => Signal s v -> (Event v -> IO ()) -> Signal s v
doEvent s f =
    signal $ \sub ->
        let onEvent e = f e >> send sub e
        in s >>: onEvent

-- | Runs a side-effecting action whenever the signal sends a value.
doNext :: Scheduler s => Signal s v -> (v -> IO ()) -> Signal s v
doNext s f =
    let f' (NextEvent x) = f x
        f' _ = return ()
    in doEvent s f'

-- | Runs a side-effecting action when the signal completes.
doCompleted :: Scheduler s => Signal s v -> IO () -> Signal s v
doCompleted s f =
    let f' CompletedEvent = f
        f' _ = return ()
    in doEvent s f'

-- | Returns a signal of the first @n@ elements.
take :: (Integral n, Scheduler s) => Signal s v -> n -> Signal s v
take s n =
    signal $ \sub -> do
        remRef <- newIORef n

        let onEvent ev@(NextEvent _) = do
                old <- atomicModifyIORef remRef $ \rem ->
                    if rem == 0 then (0, 0) else (rem - 1, rem)

                case old of
                    0 -> return ()
                    1 -> send sub ev >> send sub CompletedEvent
                    _ -> send sub ev

            onEvent ev = do
                b <- atomicModifyIORef remRef $ \rem -> (0, rem /= 0)
                when b $ send sub ev

        s >>: onEvent

-- | Returns a signal without the first @n@ elements.
drop :: (Integral n, Scheduler s) => Signal s v -> n -> Signal s v
drop s n =
    signal $ \sub -> do
        remRef <- newIORef n

        let onEvent ev@(NextEvent _) = do
                old <- atomicModifyIORef remRef $ \rem ->
                    if rem == 0 then (0, 0) else (rem - 1, rem)

                when (old == 0) $ send sub ev

            onEvent ev = send sub ev

        s >>: onEvent

-- | Returns a signal that sends the values from the most recently sent signal.
switch :: (Scheduler s, Scheduler t) => Signal s (Signal t v) -> Signal t v
switch s =
    signal $ \sub -> do
        ds <- newDisposableSet
        actives <- newIORef (True, False) -- Outer, Inner

        currD <- newIORef EmptyDisposable

        let disposeCurrD = do
                d <- readIORef currD
                dispose d

        newDisposable disposeCurrD >>= addDisposable ds

        let modifyActives :: (Maybe Bool, Maybe Bool) -> IO (Bool, Bool)
            modifyActives (Nothing, Just ni) = atomicModifyIORef actives $ \(outer, _) -> ((outer, ni), (outer, ni))
            modifyActives (Just no, Nothing) = atomicModifyIORef actives $ \(_, inner) -> ((no, inner), (no, inner))

            completeIfDone (False, False) = send sub CompletedEvent
            completeIfDone _ = return ()

            onEvent (NextEvent s') = do
                let onInnerEvent CompletedEvent = modifyActives (Nothing, Just False) >>= completeIfDone
                    onInnerEvent ev = send sub ev

                modifyActives (Nothing, Just True)
                nd <- s' >>: onInnerEvent

                oldD <- atomicModifyIORef currD (\oldD -> (nd, oldD))
                dispose oldD

            onEvent (ErrorEvent e) = send sub $ ErrorEvent e
            onEvent CompletedEvent = modifyActives (Just False, Nothing) >>= completeIfDone

        s >>: onEvent >>= addDisposable ds
        toDisposable ds

-- | Combines the latest values sent by both signals.
combine :: Scheduler s => Signal s a -> Signal s b -> Signal s (a, b)
combine a b =
    signal $ \sub -> do
        aVal <- atomically $ newTVar Nothing
        aDone <- atomically $ newTVar False

        bVal <- atomically $ newTVar Nothing
        bDone <- atomically $ newTVar False

        ds <- newDisposableSet

        let completed = do
                ac <- readTVar aDone
                bc <- readTVar bDone
                return $ if ac && bc then Just CompletedEvent else Nothing

            onEvent' _ _ (ErrorEvent e) = return $ Just $ ErrorEvent e
            onEvent' _ done CompletedEvent = writeTVar done True >> completed
            onEvent' val _ (NextEvent x) = do
                writeTVar val $ Just x

                av <- readTVar aVal
                bv <- readTVar bVal

                case (av, bv) of
                    (Just ax, Just bx) -> return $ Just $ NextEvent (ax, bx)
                    _ -> return Nothing

            onEvent val done ev = do
                m <- atomically $ onEvent' val done ev
                case m of
                    (Just ev) -> send sub ev
                    Nothing -> return ()

        a >>: onEvent aVal aDone >>= addDisposable ds
        b >>: onEvent bVal bDone >>= addDisposable ds

        toDisposable ds

-- | Synchronously waits for the signal to send an event.
first :: Scheduler s => Signal s v -> IO (Event v)
first s = do
    var <- newEmptyMVar

    take s 1 >>: void . tryPutMVar var
    ev <- takeMVar var

    return ev
