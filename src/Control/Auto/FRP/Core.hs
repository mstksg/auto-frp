{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Control.Auto.FRP.Core where

import Control.Auto
import Prelude hiding ((.), id)
import Data.String
import Data.Profunctor
import Control.Monad.Trans.Reader
import Data.Typeable

newtype Wire m a b = Wire { wireAuto :: Auto (ReaderT Double m) a b
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



integral :: Monad m => Double -> Wire m Double Double
integral = Wire . mkStateM (\dx x0 -> do dt <- ask
                                         return (x0, x0 + dx * dt)
                           )

derivative :: Monad m => Wire m Double Double
derivative = Wire $ mkStateM f Nothing
  where
    f x Nothing   = return (0, Just x)
    f x (Just x') = do
      dt <- ask
      return ((x - x')/dt, Just x)

time :: Monad m => Wire m a Double
time = integral 0 . pure 1




