{-# LANGUAGE ViewPatterns #-}

module Control.Auto.FRP.Event where

import Control.Auto               as A hiding (Interval)
import Control.Auto.Blip                      as A
import Control.Auto.Blip.Internal             as A
import Control.Auto.FRP.Core
import Data.List
import Data.Serialize
import Prelude hiding                         ((.), id)

infixr 5 `mergeL`
infixr 5 <&
infixl 5 `mergeR`
infixl 5 &>

never :: Wire m a (Event b)
never = Wire A.never

immediately :: Wire m b (Event b)
immediately = Wire A.immediately

inE :: Monad m => Time -> Wire m a (Event a)
inE = mkStatedT f . Just
  where
    f _ Nothing  _              = (NoBlip, Nothing)
    f x (Just t) dt | t <= 0    = (Blip x, Nothing)
                    | otherwise = (NoBlip, Just (t - dt))

every :: Monad m => Time -> Wire m a (Event a)
every (max 0 -> per) = mkStatedT f per
  where
    f x t dt | t <= 0    = (Blip x, per)
             | otherwise = (NoBlip, t - dt)

eachAt :: (Monad m, Serialize b) => Time -> [b] -> Wire m a (Event b)
eachAt (max 0 -> per) xs = mkStatedT (\_ -> _eachAtF per) (per, xs)

eachAt_ :: Monad m => Time -> [b] -> Wire m a (Event b)
eachAt_ (max 0 -> per) xs = mkStatedT_ (\_ -> _eachAtF per) (per, xs)

_eachAtF :: Time
         -> (Time, [b])
         -> Time
         -> (Event b, (Time, [b]))
_eachAtF per = f
  where
    f (t, xs) dt = case xs of
                     []               -> (NoBlip, (0       , xs))
                     y:ys | t <= 0    -> (Blip y, (per     , ys))
                          | otherwise -> (NoBlip, (per - dt, xs))

holdWith :: Serialize a => a -> Wire m (Event a) a
holdWith = Wire . A.holdWith

holdWith_ :: Serialize a => a -> Wire m (Event a) a
holdWith_ = Wire . A.holdWith_

merge :: (a -> a -> a) -> Event a -> Event a -> Event a
merge = A.merge

mergeL :: Event a -> Event a -> Event a
mergeL = A.mergeL

mergeR :: Event a -> Event a -> Event a
mergeR = A.mergeR

mergeLs :: [Event a] -> Event a
mergeLs = A.mergeLs

mergeRs :: [Event a] -> Event a
mergeRs = A.mergeRs

(<&) :: Monad m
     => Wire m a (Event b)
     -> Wire m a (Event b)
     -> Wire m a (Event b)
(<&) = overWire2 (A.<&)

(&>) :: Monad m
     => Wire m a (Event b)
     -> Wire m a (Event b)
     -> Wire m a (Event b)
(&>) = overWire2 (A.&>)

foldrE :: (a -> a -> a) -> [Event a] -> Event a
foldrE f = foldr (A.merge f) NoBlip

foldlE' :: (a -> a -> a) -> [Event a] -> Event a
foldlE' f = foldl' (A.merge f) NoBlip

once :: Wire m (Event a) (Event a)
once = Wire A.once

notYet :: Wire m (Event a) (Event a)
notYet = Wire A.notYet

filterE :: (a -> Bool)
        -> Wire m (Event a) (Event a)
filterE = Wire . filterB

splitE :: (a -> Bool)
       -> Wire m (Event a) (Event a, Event a)
splitE = Wire . splitB

mapMaybeE :: (a -> Maybe b)
          -> Wire m (Event a) (Event b)
mapMaybeE = Wire . mapMaybeB

takeE :: Int -> Wire m (Event a) (Event a)
takeE = Wire . takeB

takeWhileE :: (a -> Bool) -> Wire m (Event a) (Event a)
takeWhileE = Wire . takeWhileB

dropE :: Int -> Wire m (Event a) (Event a)
dropE = Wire . dropB

dropWhileE :: (a -> Bool) -> Wire m (Event a) (Event a)
dropWhileE = Wire . dropWhileB

accumE :: Serialize b
       => (b -> a -> b)
       -> b
       -> Wire m (Event a) (Event b)
accumE f = Wire . accumB f

accumE_ :: (b -> a -> b)
        -> b
        -> Wire m (Event a) (Event b)
accumE_ f = Wire . accumB_ f

scanE :: Serialize b
      => (b -> a -> b)
      -> b
      -> Wire m (Event a) b
scanE f = Wire . scanB f

scanE_ :: (b -> a -> b)
       -> b
       -> Wire m (Event a) b
scanE_ f = Wire . scanB_ f

mscanE :: (Monoid a, Serialize a) => Wire m (Event a) a
mscanE = Wire mscanB
-- should this be removed? is it worth having here?

mscanE_ :: Monoid a => Wire m (Event a) a
mscanE_ = Wire mscanB_

countE :: Wire m (Event a) Int
countE = Wire countB

tagEvents :: b -> Wire m (Event a) (Event b)
tagEvents = Wire . tagBlips

modifyEvents :: (a -> b) -> Wire m (Event a) (Event b)
modifyEvents = Wire . modifyBlips

became :: Serialize a => (a -> Bool) -> Wire m a (Event a)
became = Wire . A.became

-- TODO: remove constraint when ready
became_ :: Monad m => (a -> Bool) -> Wire m a (Event a)
became_ = Wire . A.became_

became' :: Monad m => (a -> Bool) -> Wire m a (Event ())
became' = Wire . A.became'

noLonger :: Serialize a => (a -> Bool) -> Wire m a (Event a)
noLonger = Wire . A.noLonger

noLonger_ :: Monad m => (a -> Bool) -> Wire m a (Event a)
noLonger_ = Wire . A.noLonger_

noLonger' :: Monad m => (a -> Bool) -> Wire m a (Event ())
noLonger' = Wire . A.noLonger'

onFlip :: (Serialize a, Monad m) => (a -> Bool) -> Wire m a (Event a)
onFlip = Wire . A.onFlip

onFlip_ :: Monad m => (a -> Bool) -> Wire m a (Event a)
onFlip_ = Wire . A.onFlip_

onFlip' :: Monad m => (a -> Bool) -> Wire m a (Event Bool)
onFlip' = Wire . A.onFlip'

-- iterateE :: Serialize a => a -> Wire m (Event (a -> a)) (Event a)
-- iterateE = Wire . iterateB

-- iterateE_ :: a -> Wire m (Event (a -> a)) (Event a)
-- iterateE_ = Wire . iterateB_
