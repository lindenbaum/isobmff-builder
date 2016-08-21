{-# LANGUAGE UndecidableInstances #-}
module ElementaryStreamDescriptorSpec (spec) where

import Prelude hiding ((.))
import           Control.Category
import           Data.Bits
import           Data.ByteString.Builder
import           Data.ByteString.IsoBaseFileFormat.Box
import           Data.ByteString.IsoBaseFileFormat.ReExports
import           Data.Type.BitRecords
import           Data.ByteString.IsoBaseFileFormat.Util.BoxFields
import qualified Data.ByteString.Lazy                                 as B
import           Data.ByteString.Mp4.Boxes.ElementaryStreamDescriptor
import           Data.Word
import           Test.Hspec
import           Test.QuickCheck


-- * Static Expandable

staticExpandable
  :: forall record
   . (ToM BitStringBuilder
          (Proxy (StaticExpandableContent record))
          (StaticExpandable record)
      ~ StaticExpandable record
   , ToHoley BitStringBuilder
          (Proxy (StaticExpandableContent record))
          (StaticExpandable record)
   , KnownExpandable record
   )
   => Proxy record -> StaticExpandable record
staticExpandable = runHoley . staticExpandableHoley

staticExpandableWithArgs
  :: forall record
   . ( ToHoley BitStringBuilder
          (Proxy (StaticExpandableContent record))
          (StaticExpandable record)
   , KnownExpandable record )
   => Proxy record -> ToM BitStringBuilder (Proxy (StaticExpandableContent record)) (StaticExpandable record)
staticExpandableWithArgs = runHoley . staticExpandableHoley

staticExpandableHoley
  :: forall record r
   . ( KnownExpandable record
     , ToHoley BitStringBuilder (Proxy (StaticExpandableContent record)) r)
   => Proxy record -> Holey (StaticExpandable record) r (ToM BitStringBuilder (Proxy (StaticExpandableContent record)) r)
staticExpandableHoley _ =
  hoistM StaticExpandable (bitBoxHoley (Proxy :: Proxy (StaticExpandableContent record)))

newtype StaticExpandable r =
  StaticExpandable (BitBox (StaticExpandableContent r))
  deriving (Monoid)

deriving instance (KnownExpandable r) => IsBoxContent (StaticExpandable r)

type KnownExpandable record =
  (KnownNat
    (GetRecordSize
      (StaticExpandableContent record)))

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

    it "has a boxSize of 3 when using a 16-bit body value" $
      boxSize (staticExpandable (Proxy :: Proxy (Field  16 := 1234 ))) `shouldBe` (BoxSize 3)
    it "has a boxBuilder that writes the body in big endian byte order for a 32-bit body value" $
      B.unpack (toLazyByteString (boxBuilder (staticExpandable (Proxy :: Proxy (Field  32 := 0x12345678 )))))
      `shouldBe` [4, 0x12, 0x34, 0x56, 0x78]
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
