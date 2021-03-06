module Control.Auto.FRP.Switch where

import Control.Auto          as A hiding (Interval)
import Control.Auto.FRP.Core
import Data.Serialize
import Control.Auto.Switch as A
import Prelude hiding                    ((.), id)

infixr 1 -->
infixr 1 -?>

(-->) :: Monad m => Interval m a b -> Wire m a b -> Wire m a b
(-->) = overWire2 (A.-->)

(-?>) :: Monad m => Interval m a b -> Interval m a b -> Interval m a b
(-?>) = overWire2 (A.-?>)

switchFrom_ :: Monad m => Wire m a (b, Event (Wire m a b)) -> Wire m a b
switchFrom_ (Wire a) = Wire $ A.switchFrom_ ((second . fmap) runWire <$> a)

switchOn_ :: Monad m => Wire m a b -> Wire m (a, Event (Wire m a b)) b
switchOn_ (Wire a) = Wire $ A.switchOn_ a . arr ((second . fmap) runWire)

switchOnF :: (Monad m, Serialize c)
          => (c -> Wire m a b)
          -> Wire m a b
          -> Wire m (a, Event c) b
switchOnF f (Wire a) = Wire $ A.switchOnF (runWire . f) a

switchOnF_ :: Monad m
           => (c -> Wire m a b)
           -> Wire m a b
           -> Wire m (a, Event c) b
switchOnF_ f (Wire a) = Wire $ A.switchOnF_ (runWire . f) a

switchFromF :: (Monad m, Serialize c)
            => (c -> Wire m a (b, Event c))
            -> Wire m a (b, Event c)
            -> Wire m a b
switchFromF f (Wire a) = Wire $ A.switchFromF (runWire . f) a

switchFromF_ :: Monad m
             => (c -> Wire m a (b, Event c))
             -> Wire m a (b, Event c)
             -> Wire m a b
switchFromF_ f (Wire a) = Wire $ A.switchFromF_ (runWire . f) a

resetOn :: Monad m
        => Wire m a b
        -> Wire m (a, Event c) b
resetOn = overWire A.resetOn

resetFrom :: Monad m
          => Wire m a (b, Event c)
          -> Wire m a b
resetFrom = overWire A.resetFrom
