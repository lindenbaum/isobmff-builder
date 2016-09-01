module Mp4AudioSampleEntry ( spec ) where

import           Data.ByteString.Builder
import           Data.ByteString.IsoBaseFileFormat.ReExports
import qualified Data.ByteString.Lazy                                 as B
import           Data.ByteString.Mp4.Boxes.Mp4AudioSampleEntry
import           Data.Type.BitRecords
import           Test.Hspec

spec :: Spec
spec = describe "AudioObjectType" $
    it "type-checks" $ do
        print testAudioObjectType

testAudioObjectType = Valid
testAudioObjectType ::

      "Audio Object Types"
      #####################

      "Small Audio Object Types"
      ~~~~~~~~~~~~~~~~~~~~~~~~~~~
         5 `ShouldBe` BitRecordSize (ToBitRecord (AudioObjectType 30))  -/-

      "Big Audio Object Types"
      ~~~~~~~~~~~~~~~~~~~~~~~~
        11 `ShouldBe` BitRecordSize (ToBitRecord (AudioObjectType 31))
