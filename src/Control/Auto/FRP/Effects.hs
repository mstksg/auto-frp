module Control.Auto.FRP.Effects where

import Control.Auto          as A hiding (Interval)
import Control.Auto.Effects              as A
import Data.Profunctor
import Control.Auto.FRP.Core
import Data.Serialize
import Control.Monad.Trans.Class
import Prelude hiding                    ((.), id)

arrM :: Monad m => (a -> m b) -> Wire m a b
arrM = Wire . A.arrM . fmap lift

effect :: Monad m => m b -> Wire m a b
effect = Wire . A.effect . lift

effects :: Monad m => Wire m (m a) a
effects = Wire $ lmap lift A.effects

arrMB :: Monad m => (a -> m b) -> Wire m (Event a) (Event b)
arrMB = Wire . A.arrMB . fmap lift

effectB :: Monad m => m b -> Wire m (Event a) (Event b)
effectB = Wire . A.effectB . lift

execB :: Monad m => m b -> Wire m (Event a) (Event a)
execB = Wire . A.execB . lift

cache :: (Serialize b, Monad m)
      => m b
      -> Wire m a b
cache = Wire . A.cache . lift

execOnce :: Monad m => m b -> Wire m a ()
execOnce = Wire . A.execOnce . lift

cache_ :: Monad m
       => m b
       -> Wire m a b
cache_ = Wire . A.cache_ . lift

execOnce_ :: Monad m => m b -> Wire m a ()
execOnce_ = Wire . A.execOnce_ . lift

