{-# LANGUAGE Strict #-}
module Data.Type.BitRecords.Builder.Holey where

import           Control.Category
import           Data.Monoid
import           Prelude          hiding (id, (.))
import           Data.Tagged

newtype Holey m r a = HM {runHM :: (m -> r) -> a }

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

applyHoley :: Holey m r (a -> b) -> a -> Holey m r b
applyHoley (HM !f) x = HM $ \k -> f k x

taggedHoley :: forall tag m r a x . Holey m r (a -> x) -> Holey m r (Tagged tag a -> x)
taggedHoley = mapHoley (\f -> f . untag)

-- TODO prove Functor law, make functor
mapHoley :: (a -> b) -> Holey m r a -> Holey m r b
mapHoley f (HM !h) = HM $ \k -> f (h k)

runHoley :: Holey m m a -> a
runHoley = ($ id) . runHM
