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
import Data.Foldable as F
import Data.Sequence as Seq
import Data.IORef
import Data.Unique

-- | Represents an operation which can be canceled or a resource which can be freed.
data Disposable = EmptyDisposable
                | Disposable Unique (IORef MaybeAction)

instance Eq Disposable where
    EmptyDisposable == EmptyDisposable = True
    (Disposable u _) == (Disposable u' _) = u == u'
    _ == _ = False

-- | @Just f@ when not yet disposed. @Nothing@ after disposal.
type MaybeAction = Maybe (IO ())

-- | Disposes a disposable.
dispose :: Disposable -> IO ()
dispose EmptyDisposable = return ()
dispose (Disposable _ mref) = do
    m <- atomicModifyIORef mref $ \m -> (Nothing, m)
    maybe (return ()) id m

-- | Creates a disposable which runs the given action upon disposal.
newDisposable :: IO () -> IO Disposable
newDisposable action = liftM2 Disposable newUnique $ newIORef $ Just action

-- | @Just s@ when not yet disposed. @Nothing@ after disposal.
type MaybeSet = Maybe (Seq Disposable)

-- | A synchronized set of disposables.
newtype DisposableSet = DisposableSet (IORef MaybeSet)

-- | Creates a set of disposables.
newDisposableSet :: IO DisposableSet
newDisposableSet = DisposableSet <$> newIORef (Just Seq.empty)

-- | Converts a set of disposables into a disposable.
-- | The constructed disposable will dispose of all disposables in the set.
toDisposable :: DisposableSet -> IO Disposable
toDisposable (DisposableSet mref) =
    let disposeSet :: Seq Disposable -> IO ()
        disposeSet = F.mapM_ dispose

        action :: IO ()
        action = do
            m <- atomicModifyIORef mref $ \m -> (Nothing, m)
            maybe (return ()) disposeSet m
    in newDisposable action

-- | Adds a disposable to a set.
addDisposable :: DisposableSet -> Disposable -> IO ()
addDisposable (DisposableSet mref) d =
    let addDisposable' :: MaybeSet -> (MaybeSet, Bool)
        addDisposable' Nothing = (Nothing, True)
        addDisposable' (Just s) = (Just $ s |> d, False)
    in do
        b <- atomicModifyIORef mref addDisposable'
        when b $ dispose d

-- | Removes a disposable from a set.
removeDisposable :: DisposableSet -> Disposable -> IO ()
removeDisposable (DisposableSet mref) d =
    let removeDisposable' :: MaybeSet -> MaybeSet
        removeDisposable' = liftM $ Seq.filter (/= d)
    in atomicModifyIORef mref $ \m -> (removeDisposable' m, ())
