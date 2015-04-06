module Control.Auto.FRP.Interval where

import Control.Auto               as A hiding (Interval)
import Control.Auto.Blip.Internal
import Control.Auto.FRP.Core
import Control.Auto.Interval      as A hiding (Interval, (<|?>), (<|!>))
import Data.Serialize
import Prelude hiding                         ((.), id)
import qualified Control.Auto.Interval        as A

infixr 3 <|?>
infixr 3 <|!>
infixr 1 `compI`


off :: Interval m a b
off = Wire A.off

toOn :: Interval m a a
toOn = Wire A.toOn

onFor :: Monad m => Time -> Interval m a a
onFor = mkStatedT f . Just . max 0
  where
    f x (Just t) dt | t > 0 = (Just x , Just (t - dt))
    f _ _        _          = (Nothing, Nothing      )

offFor :: Monad m => Time -> Interval m a a
offFor = mkStatedT f . Just . max 0
  where
    f _ (Just t) dt | t > 0 = (Nothing, Just (t - dt))
    f x _        _          = (Just x , Nothing      )

window :: Monad m => Time -> Time -> Interval m a a
window b e = mkStatedT f (Just 0)
  where
    f _ Nothing  _              = (Nothing, Nothing      )
    f x (Just t) dt | t > e     = (Nothing, Nothing      )
                    | t < b     = (Nothing, Just (t + dt))
                    | otherwise = (Just x , Just (t + dt))

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
holdFor t = mkStatedT (_holdForF (max 0 t)) (Nothing, Nothing)

holdFor_ :: Monad m => Time -> Interval m (Event a) a
holdFor_ t = mkStatedT_ (_holdForF (max 0 t)) (Nothing, Nothing)

_holdForF :: Time
          -> Event a
          -> (Maybe a, Maybe Time)
          -> Time
          -> (Maybe a, (Maybe a, Maybe Time))
_holdForF dur = f
  where
    f x s dt = (y, (y, t))
      where
        (y, t) = case (x, s) of
                   (Blip b, _)              -> (Just b , Just dur     )
                   (_, (z, Just j)) | j > 0 -> (z      , Just (j - dt))
                   _                        -> (Nothing, Nothing      )

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

