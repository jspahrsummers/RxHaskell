{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
                        , never
                        , Signal.empty
                        , Signal
                        ) where

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
import Signal.Subscriber

-- | Turns any Foldable into a signal.
fromFoldable :: (Foldable t, Scheduler s) => t v -> Signal s v
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
doEvent :: Scheduler s => Signal s v -> (Event v -> SchedulerIO s ()) -> Signal s v
doEvent s f =
    signal $ \sub ->
        let onEvent e = f e >> send sub e
        in s >>: onEvent

-- | Runs a side-effecting action whenever the signal sends a value.
doNext :: Scheduler s => Signal s v -> (v -> SchedulerIO s ()) -> Signal s v
doNext s f =
    let f' (NextEvent x) = f x
        f' _ = return ()
    in doEvent s f'

-- | Runs a side-effecting action when the signal completes.
doCompleted :: Scheduler s => Signal s v -> SchedulerIO s () -> Signal s v
doCompleted s f =
    let f' CompletedEvent = f
        f' _ = return ()
    in doEvent s f'

-- | Returns a signal of the first @n@ elements.
take :: (Integral n, Scheduler s) => Signal s v -> n -> Signal s v
take s n =
    signal $ \sub -> do
        remRef <- liftIO $ newIORef n

        let onEvent ev@(NextEvent _) = do
                old <- liftIO $ atomicModifyIORef remRef $ \rem ->
                    if rem == 0 then (0, 0) else (rem - 1, rem)

                case old of
                    0 -> return ()
                    1 -> send sub ev >> send sub CompletedEvent
                    _ -> send sub ev

            onEvent ev = do
                b <- liftIO $ atomicModifyIORef remRef $ \rem -> (0, rem /= 0)
                when b $ send sub ev

        s >>: onEvent

-- | Returns a signal without the first @n@ elements.
drop :: (Integral n, Scheduler s) => Signal s v -> n -> Signal s v
drop s n =
    signal $ \sub -> do
        remRef <- liftIO $ newIORef n

        let onEvent ev@(NextEvent _) = do
                old <- liftIO $ atomicModifyIORef remRef $ \rem ->
                    if rem == 0 then (0, 0) else (rem - 1, rem)

                when (old == 0) $ send sub ev

            onEvent ev = send sub ev

        s >>: onEvent

-- | Returns a signal that sends the values from the most recently sent signal.
switch :: forall s v. Scheduler s => Signal s (Signal s v) -> Signal s v
switch s =
    signal $ \sub -> do
        ds <- liftIO newDisposableSet
        actives <- liftIO $ newIORef (True, False) -- Outer, Inner

        currD <- liftIO $ newIORef EmptyDisposable

        let disposeCurrD = do
                d <- readIORef currD
                dispose d

        d <- liftIO $ newDisposable disposeCurrD
        liftIO $ ds `addDisposable` d

        let modifyActives :: (Maybe Bool, Maybe Bool) -> SchedulerIO s (Bool, Bool)
            modifyActives (Nothing, Just ni) = liftIO $ atomicModifyIORef actives $ \(outer, _) -> ((outer, ni), (outer, ni))
            modifyActives (Just no, Nothing) = liftIO $ atomicModifyIORef actives $ \(_, inner) -> ((no, inner), (no, inner))

            completeIfDone :: (Bool, Bool) -> SchedulerIO s ()
            completeIfDone (False, False) = send sub CompletedEvent
            completeIfDone _ = return ()

            onEvent :: Event (Signal s v) -> SchedulerIO s ()
            onEvent (NextEvent s') = do
                let onInnerEvent :: Event v -> SchedulerIO s ()
                    onInnerEvent CompletedEvent = modifyActives (Nothing, Just False) >>= completeIfDone
                    onInnerEvent ev = send sub ev

                modifyActives (Nothing, Just True)
                nd <- s' >>: onInnerEvent

                oldD <- liftIO $ atomicModifyIORef currD (\oldD -> (nd, oldD))
                liftIO $ dispose oldD

            onEvent (ErrorEvent e) = send sub $ ErrorEvent e
            onEvent CompletedEvent = modifyActives (Just False, Nothing) >>= completeIfDone

        d <- s >>: onEvent
        liftIO $ ds `addDisposable` d
        liftIO $ toDisposable ds

-- | Combines the latest values sent by both signals.
combine :: forall a b s. Scheduler s => Signal s a -> Signal s b -> Signal s (a, b)
combine a b =
    signal $ \sub -> do
        aVal <- liftIO $ atomically $ newTVar (Nothing :: Maybe a)
        aDone <- liftIO $ atomically $ newTVar False

        bVal <- liftIO $ atomically $ newTVar (Nothing :: Maybe b)
        bDone <- liftIO $ atomically $ newTVar False

        ds <- liftIO newDisposableSet

        let completed :: STM (Maybe (Event (a, b)))
            completed = do
                ac <- readTVar aDone
                bc <- readTVar bDone
                return $ if ac && bc then Just CompletedEvent else Nothing

            onEvent' :: TVar (Maybe x) -> TVar Bool -> Event x -> STM (Maybe (Event (a, b)))
            onEvent' _ _ (ErrorEvent e) = return $ Just $ ErrorEvent e
            onEvent' _ done CompletedEvent = writeTVar done True >> completed
            onEvent' val _ (NextEvent x) = do
                writeTVar val $ Just x

                av <- readTVar aVal
                bv <- readTVar bVal

                case (av, bv) of
                    (Just ax, Just bx) -> return $ Just $ NextEvent (ax, bx)
                    _ -> return Nothing

            onEvent :: TVar (Maybe x) -> TVar Bool -> Event x -> SchedulerIO s ()
            onEvent val done ev = do
                m <- liftIO $ atomically $ onEvent' val done ev
                case m of
                    (Just ev) -> send sub ev
                    Nothing -> return ()

        ad <- a >>: onEvent aVal aDone
        liftIO $ ds `addDisposable` ad

        bd <- b >>: onEvent bVal bDone
        liftIO $ ds `addDisposable` bd

        liftIO $ toDisposable ds
