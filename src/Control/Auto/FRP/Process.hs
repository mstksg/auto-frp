{-# LANGUAGE TupleSections #-}

module Control.Auto.FRP.Process where

import Control.Auto               as A hiding (arrM)
import Control.Auto.FRP.Core
import Control.Auto.FRP.Effects
import Control.Auto.FRP.Time
import Control.Monad
import Prelude hiding                         ((.), id)

integral :: Monad m => Double -> Wire m Double Double
integral = mkStatedT $ \dx y0 dt -> (y0, y0 + dx * dt)

derivative :: Monad m => Wire m Double Double
derivative = mkStatedT f Nothing
  where
    f x Nothing   _  = (0          , Just x)
    f x (Just x') dt = ((x - x')/dt, Just x)

integralRK4 :: Monad m
            => Double
            -> Double
            -> Double
            -> Wire m Double Double
integralRK4 y0 x0 x1 = mkStatedT f (y0, x0, x1)
  where
    f x (y0', x0', x1') dt = (y0', (outp, x1', x))
      where
        outp = y0' + dt / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
        k1   = x0'
        k2   = x1'
        k3   = x1'
        k4   = x

onTime :: Monad m => (Time -> b) -> Wire m a b
onTime f = fmap f time

onTimeFrom :: Monad m => (Time -> b) -> Time -> Wire m a b
onTimeFrom f = fmap f . timeFrom

onTimeM :: Monad m => (Time -> m b) -> Wire m a b
onTimeM f = arrM f . time

onTimeFromM :: Monad m => (Time -> m b) -> Time -> Wire m a b
onTimeFromM f t0 = arrM f . timeFrom t0

withTimeFrom :: Monad m => (a -> Time -> b) -> Time -> Wire m a b
withTimeFrom g = mkStatedT $ \x t dt -> (g x t, t + dt)

withTimeFromM :: Monad m => (a -> Time -> m b) -> Time -> Wire m a b
withTimeFromM g = mkStatedTM f
  where
    f x t dt = (, t + dt) `liftM` g x t

withTime :: Monad m => (a -> Time -> b) -> Wire m a b
withTime f = withTimeFrom f 0

withTimeM :: Monad m => (a -> Time -> m b) -> Wire m a b
withTimeM f = withTimeFromM f 0
