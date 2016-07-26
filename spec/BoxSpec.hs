module BoxSpec (spec) where

import Test.Hspec
import Data.ByteString.IsoBaseFileFormat.Boxes

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary.Get as Binary
import qualified Data.Text as T

spec :: Spec
spec =
  do describe "IsBoxContent instances" $
       do describe "IsBoxContent Data.Text.Text" $ do
             describe "boxSize" $
               it "returns string length + 1 (for the null termination byte)" $ do
                  boxSize (T.pack "") `shouldBe` 1
                  boxSize (T.pack "Hello!") `shouldBe` 7
             describe "boxBuilder of Data.Text.Text" $ do
               it "appends the null termination byte" $
                  let expected = [72,101,108,108,111,32,119,111,114,108,100,33,0]
                      actual = BL.unpack $ toLazyByteString $ boxBuilder (T.pack "Hello world!")
                      in actual `shouldBe` expected
               it "appends the null termination byte" $
                  let expected = [72,101,108,108,111,32,119,111,114,108,100,33,0]
                      actual = BL.unpack $ toLazyByteString $ boxBuilder (T.pack "Hello world!")
                      in actual `shouldBe` expected
               it "replaces embeded NULL characters with a space (0x20)" $
                  let expected = [72,101,108,108,111,32,119,111,114,108,100,33,0]
                      actual = BL.unpack $ toLazyByteString $ boxBuilder (T.pack "Hello\0world!")
                      in actual `shouldBe` expected
          describe "()" $
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

data TestBrand

instance IsBrand TestBrand where
  type BoxLayout TestBrand =
    '[ 'OnceMandatory TestParentBox1 '[ 'SomeOptional TestBox1 '[]] ]

data TestBox1

instance IsBoxType TestBox1 where
  toBoxType _ _ = StdType "tst1"

testBox1 :: Box TestBrand TestBox1
testBox1 = closedBox ()

data TestParentBox1

instance IsBoxType TestParentBox1 where
  toBoxType _ _ = StdType "par1"

testParentBox1
  :: ValidContainerBox TestBrand TestParentBox1 ts
  => Boxes TestBrand ts -> Box TestBrand TestParentBox1
testParentBox1 = containerBox
