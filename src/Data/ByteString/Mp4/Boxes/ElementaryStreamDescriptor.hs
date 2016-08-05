-- | TODO SPLIT!
module Data.ByteString.Mp4.Boxes.ElementaryStreamDescriptor where

import Data.ByteString.IsoBaseFileFormat.ReExports

-- * Expandable Classes

newtype Expandable content where -- TODO maxSize in Expandable class
  Expandable :: content -> Expandable maxSize content

isntance IsBoxContent content => IsBoxContent (Expandable content) where
  boxSize (Expandable cnt) = boxSize cnt


sizeBlocks len s = s `div` 7 + if rem s 7 == 0 then 0 else 1
type ExpandableTLoop size =


-- * Base Descriptor Class Tags
type ObjectDescrTag = 0x01
type InitialObjectDescrTag = 0x02
type ES_DescrTag = 0x03
type DecoderConfigDescrTag = 0x04
type DecSpecificInfoTag = 0x05
type SLConfigDescrTag = 0x06
type ContentIdentDescrTag = 0x07
type SupplContentIdentDescrTag = 0x08
type IPI_DescrPointerTag = 0x09
type IPMP_DescrPointerTag = 0x0A
type IPMP_DescrTag = 0x0B
type QoS_DescrTag = 0x0C
type RegistrationDescrTag = 0x0D
type ES_ID_IncTag = 0x0E
type ES_ID_RefTag = 0x0F
type MP4_IOD_Tag = 0x10
type MP4_OD_Tag = 0x11
type IPL_DescrPointerRefTag = 0x12
type ExtensionProfileLevelDescrTag = 0x13
type ProfileLevelIndicationIndexDescrTag = 0x14
type ContentClassificationDescrTag = 0x40
type KeyWordDescrTag = 0x41
type RatingDescrTag = 0x42
type LanguageDescrTag = 0x43
type ShortTextualDescrTag = 0x44
type ExpandedTextualDescrTag = 0x45
type ContentCreatorNameDescrTag = 0x46
type ContentCreationDateDescrTag = 0x47
type OCICreatorNameDescrTag = 0x48
type OCICreationDateDescrTag = 0x49
type SmpteCameraPositionDescrTag = 0x4A
type SegmentDescrTag = 0x4B
type MediaTimeDescrTag = 0x4C
type IPMP_ToolsListDescrTag = 0x60
type IPMP_ToolTag = 0x61
type M4MuxTimingDescrTag = 0x62
type M4MuxCodeTableDescrTag = 0x63
type ExtSLConfigDescrTag = 0x64
type M4MuxBufferSizeDescrTag = 0x65
type M4MuxIdentDescrTag = 0x66
type DependencyPointerTag = 0x67
type DependencyMarkerTag = 0x68
type M4MuxChannelDescrTag = 0x69

type ExtDescrTagStartRange = 0x6A
type ExtDescrTagEndRange = 0xFE
type OCIDescrTagStartRange = 0x40
type OCIDescrTagEndRange = 0x5F

-- * Base Descriptor





-- * Audio Object type -- TODO move into modules

type AudioSpecificConfig audiConfig = ()

type AudioObjectType n =
   If (n <=? 30)
      (AudioObjectTypeSmall n)
      (AudioObjectTypeExt n)

type AudioObjectTypeSmall n =
  "audioObjectType" :-> Field 5 := n

type AudioObjectTypeExt n =
       AudioObjectTypeSmall 31
  :*:  "audioObjectTypeExt" :-> Field 6 := (n - 32)

type ElementaryStreamDescriptor = Tagged "TODO" Word32

testAudioObjectType ::
      "Audio Object Types"
      #####################

      "Small Audio Object Types"
      ~~~~~~~~~~~~~~~~~~~~~~~~~~~
        5 `ShouldBe` GetRecordSize (AudioObjectType 30)  -/-

      "Big Audio Object Types"
      ~~~~~~~~~~~~~~~~~~~~~~~~
        11 `ShouldBe` GetRecordSize (AudioObjectType 32)

testAudioObjectType = Valid
