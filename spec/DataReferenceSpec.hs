module DataReferenceSpec (spec) where

import Test.Hspec
import Data.ByteString.IsoBaseFileFormat.Boxes

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary.Get as Binary
import qualified Data.Text as T

spec :: Spec
spec =
   describe "IsBoxContent" $ do
     describe "LocalMediaEntry" $ do
       describe "boxSize" $ do
         it "returns 0" $
           let actual = boxSize localMediaDataReference
               expected = drefHeader + entryField + durlHeader
               drefHeader = boxHeader + fullHeader
               entryField = 4
               durlHeader = boxHeader + fullHeader
               boxHeader = 4 + 4
               fullHeader = 4
           in actual `shouldBe` expected
