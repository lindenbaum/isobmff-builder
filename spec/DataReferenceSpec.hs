module DataReferenceSpec (spec) where

import           Data.ByteString.IsoBaseFileFormat.Boxes
import           Test.Hspec

spec :: Spec
spec =
   describe "IsBoxContent" $
     describe "LocalMediaEntry" $
       describe "boxSize" $
         it "returns 0" $
           let actual = boxSize localMediaDataReference
               expected = drefHeader + entryField + durlHeader
               drefHeader = boxHeader + fullHeader
               entryField = 4
               durlHeader = boxHeader + fullHeader
               boxHeader = 4 + 4
               fullHeader = 4
           in actual `shouldBe` expected
