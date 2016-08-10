module ElementaryStreamDescriptorSpec (spec) where

import           Data.Bits
import           Data.ByteString.Builder
import           Data.ByteString.IsoBaseFileFormat.Box
import qualified Data.ByteString.Lazy                                 as B
import           Data.ByteString.Mp4.Boxes.ElementaryStreamDescriptor
import           Data.Word
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "Expandable" $ do
    it "has a box size 130 (128 + two bytes) if the content has a size of 128 " $
      boxSize (Expandable (ETC 128)) `shouldBe` (BoxSize $ 128 + 2)
    it "write a the size 128 as [ 0b10000001, 0b00000000 ] " $
      let actual = B.unpack $ toLazyByteString (boxBuilder (Expandable (ETC 128)))
          expected = [ 129, 0 ]
      in actual `shouldBe` expected
    it "write a the size (2 ^ 21) as [ 0b10000001, 0b10000000, 0b10000000, 0b00000000 ] " $
      let actual = B.unpack $ toLazyByteString (boxBuilder (Expandable (ETC 128)))
          expected = [ 129, 128, 128, 0 ]
      in actual `shouldBe` expected
    it "A size of (2 ^ 21) is represented by [ 0b10000001, 0b10000000, 0b10000000, 0b00000000 ] " $
      let actual :: Word64
          actual =  ((((1 `shiftL` 7) + 0)  `shiftL` 7) + 0) `shiftL` 7
          expected = 2 ^ (21 :: Word64)
      in actual `shouldBe` expected
    it "writes size according to the spec" $
      property $ \etc@(ETC s) ->
        let expectedBoxSize =
              BoxSize $
              s +
                   if s < 2 ^ ( 7 :: Int) then  1
              else if s < 2 ^ (14 :: Int) then  2
              else if s < 2 ^ (21 :: Int) then  3
              else if s < 2 ^ (28 :: Int) then  4
              else if s < 2 ^ (35 :: Int) then  5
              else if s < 2 ^ (42 :: Int) then  6
              else if s < 2 ^ (49 :: Int) then  7
              else if s < 2 ^ (56 :: Int) then  8
              else if s < 2 ^ (63 :: Int) then  9
                                          else 10
            actualBoxSize = boxSize (Expandable etc)
            in
              actualBoxSize `shouldBe` expectedBoxSize



instance Arbitrary ExpandableTestContent where
  arbitrary = ETC <$> arbitrary

newtype ExpandableTestContent =
    ETC Word64
  deriving (Show, Eq)

instance IsBoxContent ExpandableTestContent where
  boxSize (ETC s) = BoxSize s
  boxBuilder _ = mempty
