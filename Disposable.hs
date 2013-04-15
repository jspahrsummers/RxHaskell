{-# LANGUAGE GADTs #-}
{-# LANGUAGE Safe #-}

module Disposable ( Disposable(EmptyDisposable)
                  , newDisposable
                  , dispose
                  , DisposableSet
                  , newDisposableSet
                  , addDisposable
                  , removeDisposable
                  , toDisposable
                  ) where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Foldable as F
import Data.Sequence as Seq
import Data.IORef
import Data.Unique

-- | Allows disposal of a resource by running an action in the monad @m@.
data Disposable where
    EmptyDisposable :: Disposable
    Disposable :: Unique -> IORef (Maybe (IO ())) -> Disposable

instance Eq Disposable where
    EmptyDisposable == EmptyDisposable = True
    (Disposable u _) == (Disposable u' _) = u == u'
    _ == _ = False

-- | Disposes a disposable.
dispose :: Disposable -> IO ()
dispose EmptyDisposable = return ()
dispose (Disposable _ mref) = do
    m <- atomicModifyIORef mref $ \m -> (Nothing, m)
    fromMaybe (return ()) m

-- | Creates a disposable which runs the given action upon disposal.
newDisposable :: IO () -> IO Disposable
newDisposable action = do
    u <- newUnique
    mref <- newIORef $ Just action
    return $ Disposable u mref

-- | @Just s@ when not yet disposed. @Nothing@ after disposal.
type MaybeSet = Maybe (Seq Disposable)

-- | A synchronized set of disposables.
newtype DisposableSet = DisposableSet (IORef MaybeSet)

-- | Creates a set of disposables.
newDisposableSet :: IO DisposableSet
newDisposableSet = do
    mref <- newIORef $ Just Seq.empty
    return $ DisposableSet mref

-- | Converts a set of disposables into a disposable.
--   The constructed disposable will dispose of all disposables in the set.
toDisposable :: DisposableSet -> IO Disposable
toDisposable (DisposableSet mref) =
    let disposeSet = F.mapM_ dispose
        action = do
            m <- atomicModifyIORef mref $ \m -> (Nothing, m)
            maybe (return ()) disposeSet m
    in newDisposable action

-- | Adds a disposable to a set.
addDisposable :: DisposableSet -> Disposable -> IO ()
addDisposable _ EmptyDisposable = return ()
addDisposable (DisposableSet mref) d =
    let addDisposable' Nothing = (Nothing, True)
        addDisposable' (Just s) = (Just $ s |> d, False)
    in do
        b <- atomicModifyIORef mref addDisposable'
        when b $ dispose d

-- | Removes a disposable from a set.
removeDisposable :: DisposableSet -> Disposable -> IO ()
removeDisposable _ EmptyDisposable = return ()
removeDisposable (DisposableSet mref) d =
    let removeDisposable' = liftM $ Seq.filter (/= d)
    in atomicModifyIORef mref $ \m -> (removeDisposable' m, ())
