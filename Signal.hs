{-# LANGUAGE Safe #-}

module RxHaskell.Signal () where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.Maybe

type Subscriber a = Maybe a -> IO ()

data Signal a = Signal (Subscriber a -> IO ())

subscribe :: Signal a -> Subscriber a -> IO ()
subscribe (Signal s) next = s next

instance Monad Signal where
    return v =
        let f next = do
                next $ Just v
                next Nothing
        in Signal f

    s >>= f = Signal $ \sub -> do
        sc <- newMVar (0 :: Integer)

        let decSubscribers :: IO ()
            decSubscribers = do
                rem <- modifyMVar sc $ \n ->
                    let n' = n - 1
                    in return (n', n')

                if rem == 0
                    then sub Nothing
                    else return ()

            onInnerNext Nothing = decSubscribers
            onInnerNext m = sub m

            onOuterNext Nothing = decSubscribers
            onOuterNext (Just v) = do
                modifyMVar_ sc $ return . (+ 1)
                subscribe (f v) onInnerNext

        subscribe s onOuterNext
