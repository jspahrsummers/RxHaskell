{-# LANGUAGE Safe #-}

module Disposable ( Disposable
                  , dispose
                  , newDisposable
                  , newCompositeDisposable
                  , empty
                  , addDisposable
                  ) where

import Control.Applicative ((<$), (<$>))
import Control.Monad
import Data.IORef

-- | A list of disposables, along with a flag indicating whether disposal already happened.
type DisposableList = (Bool, [Disposable])

-- | Represents an operation which can be canceled or a resource which can be freed.
data Disposable = ActionDisposable (IO ()) (IORef Bool)
                | CompositeDisposable (IORef DisposableList)
                | EmptyDisposable

-- | Disposes a disposable. Returns whether it was already disposed.
dispose :: Disposable -> IO Bool
dispose EmptyDisposable = return True
dispose (ActionDisposable action d) = do
    b <- atomicModifyIORef d $ \b -> (True, b)
    if b
        then b <$ action
        else return b

dispose (CompositeDisposable dl) = 
    let disposedList = (True, [])
        modify (True, _) = (disposedList, disposedList)
        modify (False, xs) = (disposedList, (False, xs))
    in do
        (b, xs) <- atomicModifyIORef dl modify
        unless b $ mapM_ dispose xs
        return b

-- | Creates a disposable which runs the given action upon disposal.
newDisposable :: IO () -> IO Disposable
newDisposable action = ActionDisposable action <$> newIORef False

-- | Creates a disposable which can dispose of other disposables.
newCompositeDisposable :: IO Disposable
newCompositeDisposable = CompositeDisposable <$> newIORef (False, [])

-- | Adds disposable @d@ to the composite disposable.
addDisposable :: Disposable -> Disposable -> IO ()
addDisposable (CompositeDisposable dl) d =
    let modify (True, _) = ((True, []), True)
        modify (False, xs) = ((False, d : xs), False)
    in do
        b <- atomicModifyIORef dl modify
        when b $ dispose d >> return ()

-- | Returns a disposable which does no work.
empty :: Disposable
empty = EmptyDisposable
