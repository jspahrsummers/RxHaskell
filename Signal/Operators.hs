{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Signal.Operators ( doEvent
                        , doNext
                        , doError
                        , doCompleted
                        , finally
                        , materialize
                        , dematerialize
                        , fromFoldable
                        , map
                        , history
                        , mapAccum
                        , filter
                        , take
                        , drop
                        , switch
                        , combine
                        , never
                        , Signal.empty
                        , Signal
                        , Event
                        ) where

import Control.Concurrent.STM
import Control.Exception hiding (finally)
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.IORef
import Data.Monoid
import Data.Sequence as Seq (Seq, empty, (|>))
import Prelude hiding (filter, take, drop, map)
import Disposable
import Scheduler
import Signal
import Signal.Subscriber

-- | Turns any 'Foldable' into a signal.
fromFoldable :: (Foldable t, Scheduler s) => t v -> Signal s v
fromFoldable = foldMap return

-- | Brings every signal event into the monad, as a 'NextEvent' containing the event itself.
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

-- | Runs a side-effecting action whenever the signal sends an error.
doError :: Scheduler s => Signal s v -> (IOException -> SchedulerIO s ()) -> Signal s v
doError s f =
    let f' (ErrorEvent ex) = f ex
        f' _ = return ()
    in doEvent s f'

-- | Runs a side-effecting action when the signal completes.
doCompleted :: Scheduler s => Signal s v -> SchedulerIO s () -> Signal s v
doCompleted s f =
    let f' CompletedEvent = f
        f' _ = return ()
    in doEvent s f'

-- | Runs a side-effecting action when the signal completes or errors.
finally :: Scheduler s => Signal s v -> SchedulerIO s () -> Signal s v
finally s f =
    let f' (NextEvent _) = return ()
        f' _ = f
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

-- | Returns a signal of mapped values.
map :: Scheduler s => Signal s v -> (v -> w) -> Signal s w
map = flip fmap

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

-- | Returns a live history of the values in @sig@.
--
--   This function is similar to @duplicate@ in a 'Comonad'.
history :: forall a s. Scheduler s => Signal s a -> Signal s (Seq a)
history sig =
    signal $ \sub -> do
        values <- liftIO $ newIORef Seq.empty

        let append :: a -> Seq a -> (Seq a, Seq a)
            append v seq =
                let seq' = seq |> v
                in (seq', seq')
        
            onEvent :: Event a -> SchedulerIO s ()
            onEvent (NextEvent v) = do
                values' <- liftIO $ atomicModifyIORef values $ append v
                send sub $ NextEvent values'

            onEvent (ErrorEvent err) = send sub $ ErrorEvent err
            onEvent CompletedEvent = send sub CompletedEvent

        sig >>: onEvent

-- | Accumulates a signal's 'history', and maps it to the values for a new signal.
--
--   This function is similar to @extend@ in a 'Comonad'.
mapAccum
    :: forall a b s. Scheduler s
    => Signal s a                   -- ^ The signal to save the history for.
                                    --   Whenever this signal sends a 'NextEvent', @f@ will be reinvoked with all values thus far (including the latest).
    -> (Seq a -> SchedulerIO s b)   -- ^ A function that maps all values so far to a new value.
    -> Signal s b                   -- ^ A signal consisting of all results from @f@.

mapAccum sig f =
    signal $ \sub -> do
        let onEvent :: Event (Seq a) -> SchedulerIO s ()
            onEvent (NextEvent values) = do
                b <- f values
                send sub $ NextEvent b

            onEvent (ErrorEvent err) = send sub $ ErrorEvent err
            onEvent CompletedEvent = send sub CompletedEvent

        history sig >>: onEvent
