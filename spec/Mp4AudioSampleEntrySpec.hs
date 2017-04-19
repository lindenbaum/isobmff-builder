module Mp4AudioSampleEntrySpec ( spec ) where

import           Data.ByteString.IsoBaseFileFormat.ReExports
import           Data.ByteString.Mp4.Boxes.AudioSpecificConfig
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
         5 `ShouldBe` BitRecordSize (AudioObjectTypeRec 'AoReserved5)  -/-

      "Big Audio Object Types"
      ~~~~~~~~~~~~~~~~~~~~~~~~
        11 `ShouldBe` BitRecordSize (AudioObjectTypeRec 'AoCustom)
