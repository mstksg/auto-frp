module Control.Auto.FRP.Process where

import Control.Auto               as A
import Control.Auto.FRP.Core
import Control.Monad.Trans.Reader
import Prelude hiding             ((.), id)

integral :: Monad m => Double -> Wire m Double Double
integral = Wire . mkStateM (\dx y0 -> asks $ \dt -> (y0, y0 + dx * dt))

derivative :: Monad m => Wire m Double Double
derivative = Wire $ mkStateM f Nothing
  where
    f x Nothing   = return (0, Just x)
    f x (Just x') = asks $ \dt -> ((x - x')/dt, Just x)

-- broken still
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

