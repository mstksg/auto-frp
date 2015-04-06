module Control.Auto.FRP.Time where

import Control.Auto          as A hiding (Interval)
import Control.Auto.FRP.Core
import Control.Monad.Trans.Reader
import Prelude hiding                    ((.), id)

time :: Monad m => Wire m a Time
time = timeFrom 0

timeFrom :: Monad m => Time -> Wire m a Time
timeFrom = Wire . accumM (\t _ -> asks (t +))

