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
                        ) where

import Control.Monad
import Data.Foldable
import Data.IORef
import Data.Monoid
import Event
import Prelude hiding (filter, take, drop)
import Disposable
import Signal
import Subscriber

-- | Turns any Foldable into a signal.
fromFoldable :: Foldable t => t a -> Signal a
fromFoldable = foldMap return

-- | Treats every signal event as a 'NextEvent' containing the event itself.
-- | This can be used to make all signal events bindable.
materialize :: Signal a -> Signal (Event a)
materialize s =
    signal $ \sub ->
        let onEvent CompletedEvent = send sub (NextEvent CompletedEvent) >> send sub CompletedEvent
            onEvent ev = send sub $ NextEvent ev
        in s >>: onEvent

-- | The inverse of 'materialize'.
dematerialize :: Signal (Event a) -> Signal a
dematerialize s =
    signal $ \sub ->
        let onEvent (NextEvent ev) = send sub ev
            onEvent _ = return ()
        in s >>: onEvent

-- | Filters the values of a signal according to a predicate.
filter :: Signal a -> (a -> Bool) -> Signal a
filter s f =
    let f' x = if f x then return x else mempty
    in s >>= f'

-- | Runs a function whenever the signal sends an event.
doEvent :: Signal a -> (Event a -> IO ()) -> Signal a
doEvent s f =
    signal $ \sub ->
        let onEvent e = f e >> send sub e
        in s >>: onEvent

-- | Runs a function on each value.
doNext :: Signal a -> (a -> IO ()) -> Signal a
doNext s f =
    let f' (NextEvent x) = f x
        f' _ = return ()
    in doEvent s f'

-- | Runs a function on completion.
doCompleted :: Signal a -> IO () -> Signal a
doCompleted s f =
    let f' CompletedEvent = f
        f' _ = return ()
    in doEvent s f'

-- | Returns a signal of the first @n@ elements.
take :: Integral n => Signal a -> n -> Signal a
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
drop :: Integral n => Signal a -> n -> Signal a
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
switch :: Signal (Signal a) -> Signal a
switch s =
    signal $ \sub -> do
        cd <- newCompositeDisposable
        actives <- newIORef (True, False) -- Outer, Inner

        currD <- newIORef empty
        newDisposable (readIORef currD >>= dispose) >>= addDisposable cd

        let modifyActives (Nothing, Just ni) = atomicModifyIORef actives $ \(outer, _) -> ((outer, ni), (outer, ni))
            modifyActives (Just no, Nothing) = atomicModifyIORef actives $ \(_, inner) -> ((no, inner), (no, inner))

            completeIfDone (False, False) = send sub CompletedEvent
            completeIfDone _ = return ()

            onEvent (NextEvent s') = do
                let onInnerEvent CompletedEvent = modifyActives (Nothing, Just False) >>= completeIfDone
                    onInnerEvent ev = send sub ev

                modifyActives (Nothing, Just True)
                nd <- s' >>: onInnerEvent

                atomicModifyIORef currD (\oldD -> (nd, oldD)) >>= dispose

            onEvent (ErrorEvent e) = send sub $ ErrorEvent e
            onEvent CompletedEvent = modifyActives (Just False, Nothing) >>= completeIfDone

        s >>: onEvent >>= addDisposable cd
        return cd
