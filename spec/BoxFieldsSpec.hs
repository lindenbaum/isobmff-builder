module BoxFieldsSpec (spec) where

import Test.Hspec
import Data.ByteString.IsoBaseFileFormat.Builder
-- import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
-- import qualified Data.Binary.Get as Binary
import Control.Exception (evaluate)

-- import qualified Data.ByteString.Builder as B import qualified Data.ByteString.Lazy as BL import
-- qualified Data.Binary.Get as Binary

spec :: Spec
spec =
  describe "Scalar, Constant, Template and ScalarArry composition" $
  describe "IsBoxContent instances" $
  do describe "example1" $
       do it "boxSize reports the correct size" $
             boxSize example1 `shouldBe` (1 + 1 + (3 * 8) + (7 * 8))
          it "crashes during rendering because of the invalid number of array elements for \"baz\"" $
             evaluate (renderBox example1') `shouldThrow` anyException
     describe "example2" $
       do it "boxSize reports the correct size" $
             boxSize example2 `shouldBe` (2 * 4 + 2 + 2 + 2 + 2 + 9 * 4 + 4 + 4)
          it "it renders the expected content" $
             renderBox example2 `shouldBe` (BL.pack [0,0,0,0,
                                                     0,0,0,0,
                                                        0,65,
                                                        0,66,
                                                         1,0,
                                                         0,0,
                                                     0,0,0,67,
                                                     0,0,0,68,
                                                     0,0,0,69,
                                                     0,0,0,70,
                                                     0,0,0,71,
                                                     0,0,0,72,
                                                     0,0,0,73,
                                                     0,0,0,74,
                                                     0,0,0,75,
                                                     0,0,0,76,
                                                     0,0,0,77])

renderBox :: IsBoxContent c => c -> BL.ByteString
renderBox = toLazyByteString . boxBuilder

type ExampleContent = Scalar Word8 "bla" :+ Constant (Scalar Word8 "blub") 123 :+ Template (ScalarArray "foos" 3 Int64) '[1, 2, 3] :+ ScalarArray "baz" 7 Word64

type ExampleContentShort = U8 "bla" :+ Constant (U8 "blub") 123 :+ Template (I64Arr "foos" 3) '[1, 2, 3] :+ U64Arr "baz" 7

example1 :: ExampleContent
example1 = Scalar 100 :+ Constant :+ Default :+ u64Arr [1,2,3]

example1' :: ExampleContentShort
example1' = example1

type Example2 isAudio =
     Constant (I32Arr "reserved" 2) '[0, 0]
  :+ Template (I16 "layer") 0
  :+ Template (I16 "alternate_group") 0
  :+ Template (I16 "volume") (If isAudio 256 0)
  :+ Constant (I16 "reserved") 0
  :+ Template (I32Arr "matrix" 9) '[65536, 0, 0, 0, 65536, 0, 0, 0, 1073741824]
  :+ I32 "width"
  :+ I32 "height"

example2 :: Example2 'True
example2 =
  Constant :+
  Custom 65 :+
  Custom 66 :+
  Default :+
  Constant :+
  Custom (i32Arr [67 .. 75]) :+
  i32 76 :+
  i32 77
