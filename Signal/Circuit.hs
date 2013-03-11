{-# LANGUAGE Safe #-}

module Signal.Circuit ( CircuitM
                      , Circuit
                      , runCircuit
                      ) where

import Control.Arrow
import Control.Category
import Control.Monad
import Control.Monad.IO.Class
import Prelude hiding ((.), id)
import Signal.Operators

-- | Describes a transformation from a signal of @b@ to a signal of @c@ in the monad @m@.
newtype CircuitM m b c = CircuitM {
    runCircuit :: SignalM m b -> SignalM m c
}

-- | A circuit in the IO monad.
type Circuit = CircuitM IO

instance MonadIO m => Category (CircuitM m) where
    (CircuitM g) . (CircuitM f) = CircuitM (g . f)
    id = arr id

instance MonadIO m => Arrow (CircuitM m) where
    arr = CircuitM . liftM
    first (CircuitM f) =
        let first' bds =
                bds >>= \(b, d) ->
                    let cs = f $ return b
                    in fmap (flip (,) d) cs
        in CircuitM first'

instance MonadIO m => ArrowChoice (CircuitM m) where
    left (CircuitM a) =
        let onNext (Left b) =
                let cs = a $ return b
                in fmap Left cs

            onNext (Right d) = return $ Right d
        in CircuitM (>>= onNext)
