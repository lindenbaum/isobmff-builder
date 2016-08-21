{-# LANGUAGE Strict #-}
module Data.Type.BitRecords.Builder.Holey where

import Data.Monoid
import Control.Category
import Prelude hiding ((.), id)

class ToHoley m f r where
  type ToM m f r
  type ToM m f r = r
  toHoley :: f -> Holey m r (ToM m f r)

newtype Holey m r a = HM {runHM :: (m -> r) -> a }

-- * Normal Holey

instance Monoid m => Category (Holey m) where
  (.) (HM f) (HM g) = HM (\k -> (f (\m1 -> g (\m2 -> k (m1 <> m2)))))
  id = HM ($ mempty)

instance Monoid m => Monoid (Holey m r r) where
  mappend = (.)
  mempty = id

hoistM :: (m -> n) -> Holey m a b -> Holey n a b
hoistM into (HM f) = HM (\k -> f (k . into))

hoistR :: (s -> r) -> Holey m r a -> Holey m s a
hoistR outof (HM f) = HM (\k -> f (outof . k))

immediate :: m -> Holey m r r
immediate m =
  HM { runHM = ($ m) }

indirect :: (a -> m) -> Holey m r (a -> r)
indirect f =
  HM { runHM = (. f) }

bind :: Holey m b c
      -> (m -> Holey n a b)
      -> Holey n a c
bind mbc fm = HM $ \ kna -> runHM mbc (($ kna) . runHM . fm)

runHoley :: Holey m m a -> a
runHoley = ($ id) . runHM
