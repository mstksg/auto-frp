{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Control.Auto.FRP.Core (
    Wire(..)
  , Wire'
  , Interval
  , Interval'
  , Time
  , Event
  , overWire
  , overWire2
  , overWire3
  ) where

import Control.Auto as A hiding   (Interval, Interval')
import Control.Monad.Trans.Reader
import Data.Profunctor
import Data.String
import Data.Typeable
import Prelude hiding             ((.), id)

type Time = Double

newtype Wire m a b = Wire { runWire :: Auto (ReaderT Time m) a b
                          } deriving ( Functor
                                     , Applicative
                                     , Category
                                     , Arrow
                                     , ArrowChoice
                                     , ArrowLoop
                                     , Profunctor
                                     , Strong
                                     , Choice
                                     , Costrong
                                     , Alternative
                                     , IsString
                                     , Monoid
                                     , Semigroup
                                     , Num
                                     , Fractional
                                     , Floating
                                     , Typeable
                                     )

type Wire' = Wire Identity

type Interval m a b = Wire m a (Maybe b)

type Interval' a b = Interval Identity a b

type Event = Blip


integral :: Monad m => Double -> Wire m Double Double
integral = Wire . mkStateM (\dx y0 -> asks $ \dt -> (y0, y0 + dx * dt))

derivative :: Monad m => Wire m Double Double
derivative = Wire $ mkStateM f Nothing
  where
    f x Nothing   = return (0, Just x)
    f x (Just x') = asks $ \dt -> ((x - x')/dt, Just x)

time :: Monad m => Wire m a Time
time = integral 0 . pure 1

integralRK4 :: Monad m
            => Double
            -> Wire m Double Double
integralRK4 y0 = Wire (mkStateM f (y0,0,0,0))
  where
    f x (y,x0,x1,x2) = do
      dt <- ask
      let k1 = x0
          k2 = x1
          k3 = x1
          k4 = x2
          y' = y + dt/6 * (k1 + 2*k2 + 2*k3 + k4)
      return (y, (y',x1,x2,x))

overWire :: ( Auto (ReaderT Time m) a0 b0 -> Auto (ReaderT Time m) a1 b1)
         -> Wire m a0 b0 -> Wire m a1 b1
overWire f (Wire a) = Wire (f a)

overWire2 :: ( Auto (ReaderT Time m) a0 b0
            -> Auto (ReaderT Time m) a1 b1
            -> Auto (ReaderT Time m) a2 b2 )
          -> Wire m a0 b0
          -> Wire m a1 b1
          -> Wire m a2 b2
overWire2 f (Wire a1) (Wire a2) = Wire (f a1 a2)

overWire3 :: ( Auto (ReaderT Time m) a0 b0
            -> Auto (ReaderT Time m) a1 b1
            -> Auto (ReaderT Time m) a2 b2
            -> Auto (ReaderT Time m) a3 b3 )
          -> Wire m a0 b0
          -> Wire m a1 b1
          -> Wire m a2 b2
          -> Wire m a3 b3
overWire3 f (Wire a1) (Wire a2) (Wire a3) = Wire (f a1 a2 a3)

(-->) :: Monad m => Interval m a b -> Wire m a b -> Wire m a b
(-->) = overWire2 (A.-->)

(-?>) :: Monad m => Interval m a b -> Interval m a b -> Interval m a b
(-?>) = overWire2 (A.-?>)

