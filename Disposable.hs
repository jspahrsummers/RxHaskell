{-# LANGUAGE Safe #-}

module Disposable ( Disposable
                  , dispose
                  , newDisposable
                  , empty
                  ) where

import Control.Applicative ((<$), (<$>))
import Data.IORef

-- | Represents an operation which can be canceled or a resource which can be freed.
data Disposable = ActionDisposable (IO ()) (IORef Bool)
                | EmptyDisposable

-- | Disposes a disposable. Returns whether it was already disposed.
dispose :: Disposable -> IO Bool
dispose EmptyDisposable = return True
dispose (ActionDisposable action d) = do
    b <- atomicModifyIORef d $ \b -> (True, b)
    if b
        then b <$ action
        else return b

-- | Creates a disposable which runs the given action upon disposal.
newDisposable :: IO () -> IO Disposable
newDisposable action = ActionDisposable action <$> newIORef False

-- | Returns a disposable which does no work.
empty :: Disposable
empty = EmptyDisposable

instance Eq Disposable where
    (ActionDisposable _ a) == (ActionDisposable _ b) = a == b
    EmptyDisposable == EmptyDisposable = True
    _ == _ = False
