module ElementaryStreamDescriptorSpec (spec) where

import           Data.ByteString.IsoBaseFileFormat.Box
import           Data.ByteString.Mp4.Boxes.ElementaryStreamDescriptor
import           Data.Word
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "Expandable" $
    it "writes size according to the spec" $
      property $ \etc@(ETC s) ->
        let bitlen = ceiling (log (fromIntegral s) / log 2 :: Double)
            bitlen :: Word8
            expectedBoxSize =
              BoxSize $
              s +
              if bitlen < 8 then  1
                else if bitlen < 15 then 2
                  else if bitlen < 22 then 3
                    else if bitlen < 29 then 4
                      else if bitlen < 36 then 5
                        else if bitlen < 43 then 6
                          else if bitlen < 50 then 7
                            else if bitlen < 57 then 8
                              else if bitlen < 64 then 9
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
