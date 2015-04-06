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

time :: Monad m => Wire m a Time
time = Wire $ accumM (\t _ -> asks (t +)) 0

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

