module BoxSpec (spec) where

import Test.Hspec
import Data.ByteString.IsoBaseFileFormat.Builder
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary.Get as Binary

spec :: Spec
spec =
  do describe "IsBoxContent instances" $
       do describe "()" $
            do describe "boxSize" $ it "returns 0" $ boxSize () `shouldBe` 0
               describe "boxBuilder" $
                 it "emits no data" $
                 BL.length (B.toLazyByteString (boxBuilder ())) `shouldBe` 0
          describe "Box" $
            do describe "boxSize" $
                 it "returns the header size if the content is empty" $
                 boxSize testBox1 `shouldBe` (4 + 4)
          describe "Boxes" $
            do describe "boxSize" $
                 do describe "a box with one nested box" $
                      do it "returns the sum of both boxSizes" $
                           boxSize (testParentBox1 $ singletonBox testBox1) `shouldBe`
                           (2 * boxSize testBox1)
                         it "returns the same value as written by boxBuilder" $
                           let b = testParentBox1 $ singletonBox testBox1
                               writtenSize =
                                 let out = toLazyByteString (boxBuilder b)
                                     getSize = Binary.runGet Binary.getWord32be
                                 in BoxSize $ fromIntegral $ getSize out
                               reportedSize = boxSize b
                           in writtenSize `shouldBe` reportedSize


data TestBox1

instance BoxRules TestBox1

instance IsBoxType' TestBox1 where
  toBoxType' _ = StdType "tst1"

testBox1 :: Box' TestBox1
testBox1 = closedBox ()


data TestParentBox1

instance BoxRules TestParentBox1

instance IsBoxType' TestParentBox1 where
  toBoxType' _ = StdType "par1"

testParentBox1 :: Boxes ts -> Box' TestParentBox1
testParentBox1 = containerBox
