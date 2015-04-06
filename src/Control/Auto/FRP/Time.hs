module Control.Auto.FRP.Time where

import Control.Auto.FRP.Core
import Prelude hiding                       ((.), id)

time :: Monad m => Wire m a Time
time = timeFrom 0

timeFrom :: Monad m => Time -> Wire m a Time
timeFrom = accumdT (\t _ -> (t +))

