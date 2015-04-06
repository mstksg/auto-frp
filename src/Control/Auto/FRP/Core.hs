{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}

module Control.Auto.FRP.Core (
    Wire(..)
  , Wire'
  , Interval
  , Interval'
  , Time
  , Event
  , runWire
  , sealWire
  , arrdT
  , arrdTM
  , accumdT
  , accumdT_
  , accumdTM
  , accumdTM_
  , mkStatedT
  , mkStatedT_
  , mkStatedTM
  , mkStatedTM_
  , overWire
  , overWire2
  , overWire3
  , delay
  , delay_
  , hoistW
  , generalizeW
  ) where

import Control.Auto as A hiding   (Interval, Interval', delay, delay_)
import Control.Auto.Effects
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Profunctor
import Data.Serialize
import Data.String
import Data.Typeable
import Prelude hiding             ((.), id)
import qualified Control.Auto     as A

type Time = Double

newtype Wire m a b = Wire { wireAuto :: Auto (ReaderT Time m) a b
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

runWire :: Monad m => Wire m a b -> Auto m (a, Time) b
runWire (Wire a) = runReaderA a

sealWire :: Monad m => Wire m a b -> Time -> Auto m a b
sealWire (Wire a) = sealReader a

arrdT :: Monad m => (a -> Time -> b) -> Wire m a b
arrdT f = Wire $ arrM (asks . f)

arrdTM :: Monad m => (a -> Time -> m b) -> Wire m a b
arrdTM f = Wire . arrM $ \x -> lift . f x =<< ask

accumdT :: (Serialize b, Monad m) => (b -> a -> Time -> b) -> b -> Wire m a b
accumdT f = Wire . accumM (\s -> asks . f s)

accumdT_ :: Monad m => (b -> a -> Time -> b) -> b -> Wire m a b
accumdT_ f = Wire . accumM_ (\s -> asks . f s)

accumdTM :: (Serialize b, Monad m) => (b -> a -> Time -> m b) -> b -> Wire m a b
accumdTM f = Wire . accumM (\s x -> lift . f s x =<< ask)

accumdTM_ :: Monad m => (b -> a -> Time -> m b) -> b -> Wire m a b
accumdTM_ f = Wire . accumM_ (\s x -> lift . f s x =<< ask)

mkStatedT :: (Serialize s, Monad m) => (a -> s -> Time -> (b, s)) -> s -> Wire m a b
mkStatedT f = Wire . mkStateM (\x -> asks . f x)

mkStatedT_ :: Monad m => (a -> s -> Time -> (b, s)) -> s -> Wire m a b
mkStatedT_ f = Wire . mkStateM_ (\x -> asks . f x)

mkStatedTM :: (Serialize s, Monad m) => (a -> s -> Time -> m (b, s)) -> s -> Wire m a b
mkStatedTM f = Wire . mkStateM (\x s -> lift . f x s =<< ask)

mkStatedTM_ :: Monad m => (a -> s -> Time -> m (b, s)) -> s -> Wire m a b
mkStatedTM_ f = Wire . mkStateM_ (\x s -> lift . f x s =<< ask)


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

delay :: Serialize a => a -> Wire m a a
delay = Wire . A.delay

delay_ :: a -> Wire m a a
delay_ = Wire . A.delay_

hoistW :: (Monad m, Monad m')
       => (forall c. m c -> m' c)
       -> Wire m  a b
       -> Wire m' a b
hoistW nt (Wire a) = Wire $ hoistA (mapReaderT nt) a

generalizeW :: Monad m => Wire' a b -> Wire m a b
generalizeW = hoistW (return . runIdentity)

