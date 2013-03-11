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
                        , never
                        , Signal.empty
                        , SignalM
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
import Signal
import Subscriber

-- | Turns any Foldable into a signal.
fromFoldable :: (Foldable t, MonadIO m) => t v -> SignalM m v
fromFoldable = foldMap return

-- | Treats every signal event as a 'NextEvent' containing the event itself.
-- | This can be used to make all signal events bindable.
materialize :: MonadIO m => SignalM m v -> SignalM m (Event v)
materialize s =
    signal $ \sub ->
        let onEvent CompletedEvent = send sub (NextEvent CompletedEvent) >> send sub CompletedEvent
            onEvent ev = send sub $ NextEvent ev
        in s >>: onEvent

-- | The inverse of 'materialize'.
dematerialize :: MonadIO m => SignalM m (Event v) -> SignalM m v
dematerialize s =
    signal $ \sub ->
        let onEvent (NextEvent ev) = send sub ev
            onEvent _ = return ()
        in s >>: onEvent

-- | Filters the values of a signal according to a predicate.
filter :: MonadIO m => SignalM m v -> (v -> Bool) -> SignalM m v
filter s f =
    let f' x = if f x then return x else mempty
    in s >>= f'

-- | Runs a side-effecting action whenever the signal sends an event.
doEvent :: MonadIO m => SignalM m v -> (Event v -> IO ()) -> SignalM m v
doEvent s f =
    signal $ \sub ->
        let onEvent e = liftIO (f e) >> send sub e
        in s >>: onEvent

-- | Runs a side-effecting action whenever the signal sends a value.
doNext :: MonadIO m => SignalM m v -> (v -> IO ()) -> SignalM m v
doNext s f =
    let f' (NextEvent x) = f x
        f' _ = return ()
    in doEvent s f'

-- | Runs a side-effecting action when the signal completes.
doCompleted :: MonadIO m => SignalM m v -> IO () -> SignalM m v
doCompleted s f =
    let f' CompletedEvent = f
        f' _ = return ()
    in doEvent s f'

-- | Returns a signal of the first @n@ elements.
take :: (Integral n, MonadIO m) => SignalM m v -> n -> SignalM m v
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
drop :: (Integral n, MonadIO m) => SignalM m v -> n -> SignalM m v
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
switch :: MonadIO m => SignalM m (SignalM m v) -> SignalM m v
switch s =
    signal $ \sub -> do
        ds <- newDisposableSet
        actives <- liftIO $ newIORef (True, False) -- Outer, Inner

        currD <- liftIO $ newIORef EmptyDisposable

        let disposeCurrD = do
                d <- liftIO $ readIORef currD
                dispose d

        newDisposable disposeCurrD >>= addDisposable ds

        let modifyActives :: MonadIO m => (Maybe Bool, Maybe Bool) -> m (Bool, Bool)
            modifyActives (Nothing, Just ni) = liftIO $ atomicModifyIORef actives $ \(outer, _) -> ((outer, ni), (outer, ni))
            modifyActives (Just no, Nothing) = liftIO $ atomicModifyIORef actives $ \(_, inner) -> ((no, inner), (no, inner))

            completeIfDone (False, False) = send sub CompletedEvent
            completeIfDone _ = return ()

            onEvent (NextEvent s') = do
                let onInnerEvent CompletedEvent = modifyActives (Nothing, Just False) >>= completeIfDone
                    onInnerEvent ev = send sub ev

                modifyActives (Nothing, Just True)
                nd <- s' >>: onInnerEvent

                oldD <- liftIO $ atomicModifyIORef currD (\oldD -> (nd, oldD))
                dispose oldD

            onEvent (ErrorEvent e) = send sub $ ErrorEvent e
            onEvent CompletedEvent = modifyActives (Just False, Nothing) >>= completeIfDone

        s >>: onEvent >>= addDisposable ds
        toDisposable ds

-- | Combines the latest values sent by both signals.
combine :: MonadIO m => SignalM m a -> SignalM m b -> SignalM m (a, b)
combine a b =
    signal $ \sub -> do
        aVal <- liftIO $ atomically $ newTVar Nothing
        aDone <- liftIO $ atomically $ newTVar False

        bVal <- liftIO $ atomically $ newTVar Nothing
        bDone <- liftIO $ atomically $ newTVar False

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
                m <- liftIO $ atomically $ onEvent' val done ev
                case m of
                    (Just ev) -> send sub ev
                    Nothing -> return ()

        a >>: onEvent aVal aDone >>= addDisposable ds
        b >>: onEvent bVal bDone >>= addDisposable ds

        toDisposable ds
