{-# LANGUAGE UndecidableInstances #-}
module ElementaryStreamDescriptorSpec (spec) where

import Prelude hiding ((.))
import           Control.Category
import           Data.Bits
import           Data.ByteString.Builder
import           Data.ByteString.IsoBaseFileFormat.Box
import           Data.ByteString.IsoBaseFileFormat.ReExports
import           Data.Type.BitRecords
import qualified Data.ByteString.Lazy                                 as B
import           Data.ByteString.Mp4.Boxes.ElementaryStreamDescriptor
import           Data.Word
import           Test.Hspec
import           Test.QuickCheck

-- * Static BoxContent

newtype StaticBoxContent record where
  StaticBoxContent :: Builder -> StaticBoxContent record

instance
      ( KnownNat (GetRecordSize content) )
   => IsBoxContent (StaticBoxContent content) where
  boxSize cnt =
    -- convert from bits to bytes
    fromIntegral (getRecordSizeFromProxy cnt `unsafeShiftR` 3)
  boxBuilder (StaticBoxContent cnt) = cnt


type KnownRecord rec = (KnownNat (GetRecordSize rec)) -- TODO move to core bit record module

type BitRecordBuilder rec =  -- TODO move to ByteStringBuilder
  BitBuilder (SelectAlignmentUnsafe (GetRecordSize rec)) 0 0

type StaticBoxContentHoley rec a =
  Holey (BitRecordBuilder rec) (StaticBoxContent rec) a


type TestBox = "test" :=> Field 8

mkTestBox
  :: Holey (BitRecordBuilder TestBox) r (Tagged "test" Integer -> r)
mkTestBox =
  toHoley (Proxy :: Proxy TestBox)

toStaticBoxContent :: Num (BitBuffer a) => BitBuilder a 0 0 -> StaticBoxContent rec
toStaticBoxContent = StaticBoxContent . toBuilder



-- addBitBuilder
--   :: (IsBitBuffer (BitBuffer buffAlign))
--   => Holey Builder r (StaticBoxContent record)
--   -> Holey (BitBuilder buffAlign 0 0) r r
--   -> Holey Builder r a
-- addBitBuilder f g = f . (hoistM (appBitBuilder mempty)  g)


-- statix box content
-- toBoxContentSpec =
--   describe "Adding compile time generated content to runtime determined content" $
--     do
--       let sbc :: StaticBoxContent

-- * Static Expandable

staticExpandable :: proxy record -> StaticExpandable record
staticExpandable _ = StaticExpandable (StaticBoxContent mempty) -- TODO



---

newtype StaticExpandable r =
  StaticExpandable (StaticBoxContent (StaticExpandableContent r))

deriving instance (KnownExpandable r) => IsBoxContent (StaticExpandable r)

type KnownExpandable r =
  (KnownNat
    (GetRecordSize
      (ExpandableSize (ShiftR 64 (GetRecordSize r) 3)  :>: r)))

type StaticExpandableContent record =
  ExpandableSize (ShiftR 64 (GetRecordSize record) 3) :>: record

type family ExpandableSize (s :: Nat) where
  ExpandableSize 0 = Field 0
  ExpandableSize s =
    If (s <=? 127)
      (                                       ExpandableSizeLastChunk s)
      (ExpandableSizeNext (ShiftR 64 s 7) :>: ExpandableSizeLastChunk s)

type ExpandableSizeLastChunk (s :: Nat) = Flag := 0 :>: (Field 7 := s)

type family ExpandableSizeNext (s :: Nat) where
  ExpandableSizeNext 0 = Field 0
  ExpandableSizeNext s =
    If (s <=? 127)
      (                                        ExpandableSizeNextChunk s)
      (ExpandableSizeNext (ShiftR 64 s 7) :>:  ExpandableSizeNextChunk s)

type ExpandableSizeNextChunk (s :: Nat) = Flag := 1 :>: (Field 7 := s)

---


---

spec :: Spec
spec = do
  describe "StaticExpandable" $ do
    describe "ExpandableSizeLastChunk" $
      it "renders both 2 and 130 as 00000010 " $ do
        let actual130 = showRecord (Proxy :: Proxy (ExpandableSizeLastChunk 130))
            actual2 = showRecord (Proxy :: Proxy (ExpandableSizeLastChunk 2))
        actual130 `shouldBe` actual2
        actual2 `shouldBe` "00000010"
    describe "ExpandableSize" $
      it "creates a stdandard conform size representation for the size 130" $
        let actualStr = showRecord (Proxy :: Proxy (ExpandableSize 130))
        in actualStr `shouldBe` "1000000100000010"

    it "has a box size 130 (128 + two bytes) if the content has a size of 128 " $
      boxSize (staticExpandable (Proxy :: Proxy (Field (128 * 8) := 0))) `shouldBe` (BoxSize $ 128 + 2)
--     it "writes the size 128 as [ 0b10000001, 0b00000000 ] " $
--       let actual = B.unpack $ toLazyByteString (boxBuilder (Expandable (ETC 128)))
--           expected = [ 129, 0 ]
--       in actual `shouldBe` expected
--     it "writes the size (2 ^ 21) as [ 0b10000001, 0b10000000, 0b10000000, 0b00000000 ] " $
--       let actual = B.unpack $ toLazyByteString (boxBuilder (Expandable (ETC 128)))
--           expected = [ 129, 128, 128, 0 ]
--       in actual `shouldBe` expected
    it "A size of (2 ^ 21) is represented by [ 0b10000001, 0b10000000, 0b10000000, 0b00000000 ] " $
      let actual :: Word64
          actual =  ((((1 `shiftL` 7) + 0)  `shiftL` 7) + 0) `shiftL` 7
          expected = 2 ^ (21 :: Word64)
      in actual `shouldBe` expected
--     it "writes size according to the spec" $
--       property $ \etc@(ETC s) ->
--         let expectedBoxSize =
--               BoxSize $
--               s +
--                    if s < 2 ^ ( 7 :: Int) then  1
--               else if s < 2 ^ (14 :: Int) then  2
--               else if s < 2 ^ (21 :: Int) then  3
--               else if s < 2 ^ (28 :: Int) then  4
--               else if s < 2 ^ (35 :: Int) then  5
--               else if s < 2 ^ (42 :: Int) then  6
--               else if s < 2 ^ (49 :: Int) then  7
--               else if s < 2 ^ (56 :: Int) then  8
--               else if s < 2 ^ (63 :: Int) then  9
--                                           else 10
--             actualBoxSize = boxSize (Expandable etc)
--             in
--               actualBoxSize `shouldBe` expectedBoxSize
--
--
-- instance Arbitrary ExpandableTestContent where
--   arbitrary = ETC <$> arbitrary
--
-- newtype ExpandableTestContent =
--     ETC Word64
--   deriving (Show, Eq)
--
-- instance IsBoxContent ExpandableTestContent where
--   boxSize (ETC s) = BoxSize s
--   boxBuilder _ = mempty
