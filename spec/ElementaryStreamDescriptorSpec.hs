module ElementaryStreamDescriptorSpec ( spec ) where

import           Data.ByteString.Builder
import           Data.ByteString.IsoBaseFileFormat.ReExports
import qualified Data.ByteString.Lazy                                 as B
import           Data.ByteString.Mp4.Boxes.ElementaryStreamDescriptor
import           Data.Type.BitRecords
import           Test.Hspec

spec :: Spec
spec = describe "The Set of Type Functions" $
    it "is sound" $ do
        print testAudioObjectType

testAudioObjectType = Valid
testAudioObjectType ::
      "Audio Object Types"
      #####################

      "Small Audio Object Types"
      ~~~~~~~~~~~~~~~~~~~~~~~~~~~
        5 `ShouldBe` BitRecordSize (AudioObjectType 30)  -/-

      "Big Audio Object Types"
      ~~~~~~~~~~~~~~~~~~~~~~~~
        11 `ShouldBe` BitRecordSize (AudioObjectType 32)
