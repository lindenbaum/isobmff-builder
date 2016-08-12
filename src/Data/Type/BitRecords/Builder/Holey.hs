module Data.Type.BitRecords.Builder.Holey where
  -- TODO

import Data.Monoid
import Control.Category
import Data.Kind hiding (type (*))
import Prelude hiding ((.), id)

class ToHoley m f r where
  type ToM m f r
  type ToM m f r = r
  toHoley :: proxy f -> Holey m r (ToM m f r)

newtype Holey m r a = HM {runHM :: (m -> r) -> a }

-- * Indexec Monoid

class IxMonoid (m :: k -> k -> Type)  where
  ixEmpty :: m i i
  ixAppend :: m h i -> m i j -> m h j

newtype IxEndo (a :: k -> Type) (i :: k) (j:: k) where
  IxEndo :: { appIxEndo :: a i -> a j } -> IxEndo a i j

instance IxMonoid (IxEndo (a :: k -> Type)) where
  ixEmpty = IxEndo id
  ixAppend (IxEndo f) (IxEndo g) = IxEndo (g . f)

(%) :: IxMonoid (m :: k ->  k -> Type) =>
  Holey (m h i) b c ->
  Holey (m i j) a b ->
  Holey (m h j) a c
(%) (HM f) (HM g) =
  HM (\k -> (f (\m1 -> g (\m2 -> k (m1 `ixAppend` m2)))))

-- * Normal Holey

instance Monoid m => Category (Holey m) where
  (.) (HM f) (HM g) = HM (\k -> (f (\m1 -> g (\m2 -> k (m1 <> m2)))))
  id = HM ($ mempty)

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