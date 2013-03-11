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
import Control.Monad.IO.Class
import Data.Maybe
import Data.Foldable as F
import Data.Functor.Identity
import Data.Sequence as Seq
import Data.IORef
import Data.Unique

-- | Allows disposal of a resource by running an action in the monad @m@.
data Disposable m where
    EmptyDisposable :: Disposable m
    Disposable :: MonadIO m => Unique -> IORef (Maybe (m ())) -> Disposable m

instance Eq (Disposable m) where
    EmptyDisposable == EmptyDisposable = True
    (Disposable u _) == (Disposable u' _) = u == u'
    _ == _ = False

-- | Disposes a disposable.
dispose :: MonadIO m => Disposable m -> m ()
dispose EmptyDisposable = return ()
dispose (Disposable _ mref) = do
    m <- liftIO $ atomicModifyIORef mref $ \m -> (Nothing, m)
    fromMaybe (return ()) m

-- | Creates a disposable which runs the given action upon disposal.
newDisposable :: MonadIO m => m () -> m (Disposable m)
newDisposable action = do
    u <- liftIO newUnique
    mref <- liftIO $ newIORef $ Just action
    return $ Disposable u mref

-- | @Just s@ when not yet disposed. @Nothing@ after disposal.
type MaybeSet m = Maybe (Seq (Disposable m))

-- | A synchronized set of disposables.
newtype DisposableSet m = DisposableSet (IORef (MaybeSet m))

-- | Creates a set of disposables.
newDisposableSet :: MonadIO m => m (DisposableSet m)
newDisposableSet = do
    mref <- liftIO $ newIORef $ Just Seq.empty
    return $ DisposableSet mref

-- | Converts a set of disposables into a disposable.
-- | The constructed disposable will dispose of all disposables in the set.
toDisposable :: MonadIO m => DisposableSet m -> m (Disposable m)
toDisposable (DisposableSet mref) =
    let disposeSet = F.mapM_ dispose
        action = do
            m <- liftIO $ atomicModifyIORef mref $ \m -> (Nothing, m)
            maybe (return ()) disposeSet m
    in newDisposable action

-- | Adds a disposable to a set.
addDisposable :: MonadIO m => DisposableSet m -> Disposable m -> m ()
addDisposable (DisposableSet mref) d =
    let addDisposable' Nothing = (Nothing, True)
        addDisposable' (Just s) = (Just $ s |> d, False)
    in do
        b <- liftIO $ atomicModifyIORef mref addDisposable'
        when b $ dispose d

-- | Removes a disposable from a set.
removeDisposable :: MonadIO m => DisposableSet m -> Disposable m -> m ()
removeDisposable (DisposableSet mref) d =
    let removeDisposable' = liftM $ Seq.filter (/= d)
    in liftIO $ atomicModifyIORef mref $ \m -> (removeDisposable' m, ())
