{-# LANGUAGE Safe #-}

module Disposable ( Disposable
                  , dispose
                  , newDisposable
                  ) where

import Control.Applicative
import Data.IORef

-- | Represents an operation which can be canceled or a resource which can be freed.
data Disposable = Disposable (IO ()) (IORef Bool)

-- | Disposes a disposable. Returns whether it was already disposed.
dispose :: Disposable -> IO Bool
dispose (Disposable action d) = do
    b <- atomicModifyIORef d $ \b -> (True, b)
    if b
        then b <$ action
        else return b

-- | Creates a disposable which runs the given action upon disposal.
newDisposable :: IO () -> IO Disposable
newDisposable action = Disposable action <$> newIORef False

instance Eq Disposable where
    (Disposable _ a) == (Disposable _ b) = a == b
