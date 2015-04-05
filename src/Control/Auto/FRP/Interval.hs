module Control.Auto.FRP.Interval where

import Control.Auto               as A hiding (Interval)
import Control.Auto.Blip.Internal
import Control.Auto.FRP.Core
import Control.Auto.Interval      as A hiding (Interval, (<|?>), (<|!>))
import Control.Monad.Trans.Reader
import Data.Serialize
import Prelude hiding                         ((.), id)
import qualified Control.Auto.Interval        as A

off :: Interval m a b
off = Wire A.off

toOn :: Interval m a a
toOn = Wire A.toOn

onFor :: Monad m => Time -> Interval m a a
onFor = Wire . mkStateM f . Just . max 0
  where
    f x (Just t) | t > 0 = asks $ \dt -> (Just x, Just (t - dt))
    f _ _        = return (Nothing, Nothing)

offFor :: Monad m => Time -> Interval m a a
offFor = Wire . mkStateM f . Just . max 0
  where
    f _ (Just t) | t > 0 = asks $ \dt -> (Nothing, Just (t - dt))
    f x _        = return (Just x, Nothing)

window :: Monad m => Time -> Time -> Interval m a a
window b e = Wire $ mkStateM f (Just 0)
  where
    f _ Nothing              = return (Nothing, Nothing)
    f x (Just t) | t > e     = return (Nothing, Nothing)
                 | t < b     = asks $ \dt -> (Nothing, Just (t + dt))
                 | otherwise = asks $ \dt -> (Just x, Just (t + dt))

after :: Interval m (a, Event b) a
after = Wire A.after

before :: Interval m (a, Event b) a
before = Wire A.before

between :: Interval m (a, (Event b, Event c)) a
between = Wire A.between

hold :: Serialize a => Interval m (Event a) a
hold = Wire A.hold

hold_ :: Interval m (Event a) a
hold_ = Wire A.hold_

holdFor :: (Serialize a, Monad m) => Time -> Interval m (Event a) a
holdFor t = Wire $ mkStateM (_holdForF (max 0 t)) (Nothing, Nothing)

holdFor_ :: Monad m => Time -> Interval m (Event a) a
holdFor_ t = Wire $ mkStateM_ (_holdForF (max 0 t)) (Nothing, Nothing)

_holdForF :: Monad m
          => Time
          -> Event a
          -> (Maybe a, Maybe Time)
          -> ReaderT Time m (Maybe a, (Maybe a, Maybe Time))
_holdForF dur = f
  where
    f x s = asks $ \dt -> let (y, t) = case (x, s) of
                                         (Blip b, _)      -> (Just b , Just dur)
                                         (_, (z, Just j)) | j > 0 -> (z, Just (j - dt))
                                         _                -> (Nothing, Nothing )
                          in  (y, (y, t))

holdJusts :: Serialize a => Interval m (Maybe a) a
holdJusts = Wire A.holdJusts

holdJusts_ :: Interval m (Maybe a) a
holdJusts_ = Wire A.holdJusts_

during :: Monad m => Wire m a b -> Interval m (Maybe a) b
during = overWire A.during

compI :: Monad m => Interval m b c -> Interval m a b -> Interval m a c
compI = overWire2 A.compI

bindI :: Monad m => Interval m a b -> Interval m (Maybe a) b
bindI = overWire A.bindI

(<|!>) :: Monad m => Interval m a b -> Wire m a b -> Wire m a b
(<|!>) = overWire2 (A.<|!>)

(<|?>) :: Monad m => Interval m a b -> Interval m a b -> Interval m a b
(<|?>) = overWire2 (A.<|?>)

chooseInterval :: Monad m => [Interval m a b] -> Interval m a b
chooseInterval = foldr (<|?>) (pure Nothing)

choose :: Monad m => Wire m a b -> [Interval m a b] -> Wire m a b
choose = foldr (<|!>)

