{-# LANGUAGE UndecidableInstances #-}
module ExpandableSpec (spec) where


import           Data.ByteString.Builder
import           Data.ByteString.IsoBaseFileFormat.Box
import           Data.ByteString.IsoBaseFileFormat.ReExports
import           Data.Type.BitRecords
import qualified Data.ByteString.Lazy                                 as B
import           Data.ByteString.Mp4.Boxes.Expandable
import           Test.Hspec
import           Test.QuickCheck


spec :: Spec
spec = do
  describe "StaticExpandable" $ do
    describe "ExpandableSizeLastChunk" $
      it "renders both 2 and 130 as 00000010 " $ do
        let actual130 = bitStringPrinter (Proxy :: Proxy (ExpandableSizeLastChunk 130))
            actual2 = bitStringPrinter (Proxy :: Proxy (ExpandableSizeLastChunk 2))
        actual130 `shouldBe` actual2
        actual2 `shouldBe` "<< 02 >>"
    describe "ExpandableSize" $
      it "creates a stdandard conform size representation for the size 130" $
        let actualStr = bitStringPrinter (Proxy :: Proxy (ExpandableSize 130))
        in actualStr `shouldBe` "<< 81 02 >>"

    it "has a boxSize of 3 when using a 16-bit body value" $
      boxSize (staticExpandable (Proxy :: Proxy (Field 16 := 1234 ))) `shouldBe` BoxSize 3
    it "has a boxBuilder that writes the body in big endian byte order for a 32-bit body value" $
      B.unpack (toLazyByteString (boxBuilder (staticExpandable (Proxy :: Proxy (Field  32 := 0x12345678 )))))
      `shouldBe` [4, 0x12, 0x34, 0x56, 0x78]
    it "writes the size 128 as [ 0b10000001, 0b00000000 ] " $
      let actual = B.unpack $ toLazyByteString (boxBuilder (Expandable (ETC 128)))
          expected = [ 129, 0 ]
      in actual `shouldBe` expected
    it "writes the size (2 ^ 21) as [ 0b10000001, 0b10000000, 0b10000000, 0b00000000 ] " $
      let actual = B.unpack $ toLazyByteString (boxBuilder (Expandable (ETC (2^(21 :: Int)))))
          expected = [ 129, 128, 128, 0 ]
      in actual `shouldBe` expected
    it "writes the size (2 ^ 21 - 1) as [ 0b11111111, 0b11111111, 0b01111111 ] " $
      let actual = B.unpack $ toLazyByteString (boxBuilder (Expandable (ETC (2^(21 :: Int) - 1))))
          expected = [ 255, 255, 127 ]
      in actual `shouldBe` expected
    it "writes size according to the spec" $
      property $ \etc@(ETC s) ->
        let expectedBoxSize =
              BoxSize $
              s +
                   if s < 2 ^ ( 7 :: Int) then  1
              else if s < 2 ^ (14 :: Int) then  2
              else if s < 2 ^ (21 :: Int) then  3 else 4
            actualBoxSize = boxSize (Expandable etc)
            in
              actualBoxSize `shouldBe` expectedBoxSize


instance Arbitrary ExpandableTestContent where
  arbitrary = ETC <$> choose (0, 2^(28::Integer) - 1)

newtype ExpandableTestContent =
    ETC Word64
  deriving (Show, Eq)

instance IsBoxContent ExpandableTestContent where
  boxSize (ETC s) = BoxSize s
  boxBuilder _ = mempty
