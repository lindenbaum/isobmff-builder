{-# LANGUAGE UndecidableInstances #-}
module Data.ByteString.Mp4.Boxes.DecoderConfigDescriptor where

import           Data.ByteString.Mp4.Boxes.BaseDescriptor
import           Data.Type.BitRecords
import           Data.Type.Pretty
import           GHC.TypeLits

-- * Information about what decoder is required for the an elementary stream.
-- The stream type indicates the general category of the stream and.
type family DecoderConfigDescriptor decInfos (ps :: [ProfileLevelIndicationIndexDescriptor]) :: BitRecord where
  -- TODO make the way decInfo is passes more elegent, use Maybe or Either
  DecoderConfigDescriptor (RecArray decInfo decInfoCount) ps =
    BaseDescriptor
     'DecoderConfigDescr
     ('ReplacePretty
       ("decoder-config-descriptor" <:$$-->
           "objectTypeIndication" <:> PutHex8 (FromEnum ObjectTypeIndication (GetObjectTypeIndication decInfo)) <$$>
           "streamType"           <:> PutHex8 (FromEnum StreamType           (GetStreamType decInfo))           <$$>
           PrettyRecord (DecoderConfigDescriptorBody (RecArray decInfoCount decInfo) ps))
       (DecoderConfigDescriptorBody
         (RecArray decInfo (decInfoCount ?:: NatIn 0 1))
         (ps ?:: LengthIn 0 255)))

type family DecoderConfigDescriptorBody decInfos (ps :: [ProfileLevelIndicationIndexDescriptor]) :: BitRecord where
  DecoderConfigDescriptorBody (RecArray decInfo decInfoCount) ps =
        EnumField ObjectTypeIndication := (GetObjectTypeIndication decInfo)
    :>: EnumField StreamType           := (GetStreamType decInfo)
    :>: "upstream"     :=> Flag
    :>: Field 1        :=  1
    :>: "bufferSizeDB" :=> Field 24
    :>: "maxBitrate"   :=> FieldU32
    :>: "avgBitrate"   :=> FieldU32
    :>: (RecArray decInfoCount decInfo)
    :>: ps

type family
  GetObjectTypeIndication (t :: k) :: ObjectTypeIndication

data ObjectTypeIndication =
    SystemsIso14496_1_a
  | SystemsIso14496_1_b
  | InteractionStreamObjInd
  | SystemsIso14496_1_ExtendedBifs
  | SystemsIso14496_1_Afx
  | FontDataStream
  | SynthesizedTextureStream
  | StreamingTextStream
  | VisualIso14496_2
  | VisualH264
  | VisualH264ParameterSets
  | AudioIso14496_3
  | VisualIso13818_2_SimpleProfile
  | VisualIso13818_2_MainProfile
  | VisualIso13818_2_SnrProfile
  | VisualIso13818_2_SpatialProfile
  | VisualIso13818_2_HighProfile
  | VisualIso13818_2_422Profile
  | AudioIso13818_7_MainProfile
  | AudioIso13818_7_LowComplexityProfile
  | AudioIso13818_7_ScalableSamplingRateProfile
  | AudioIso13818_3
  | VisualIso11172_2
  | AudioIso11172_3
  | VisualIso10918_1
  | VisualIso15444_1
  | NoObjectTypeSpecified

type instance FromEnum ObjectTypeIndication 'SystemsIso14496_1_a                         = 0x01
type instance FromEnum ObjectTypeIndication 'SystemsIso14496_1_b                         = 0x02
type instance FromEnum ObjectTypeIndication 'InteractionStreamObjInd                     = 0x03
type instance FromEnum ObjectTypeIndication 'SystemsIso14496_1_ExtendedBifs              = 0x04
type instance FromEnum ObjectTypeIndication 'SystemsIso14496_1_Afx                       = 0x05
type instance FromEnum ObjectTypeIndication 'FontDataStream                              = 0x06
type instance FromEnum ObjectTypeIndication 'SynthesizedTextureStream                    = 0x07
type instance FromEnum ObjectTypeIndication 'StreamingTextStream                         = 0x08
type instance FromEnum ObjectTypeIndication 'VisualIso14496_2                            = 0x20
type instance FromEnum ObjectTypeIndication 'VisualH264                                  = 0x21
type instance FromEnum ObjectTypeIndication 'VisualH264ParameterSets                     = 0x22
type instance FromEnum ObjectTypeIndication 'AudioIso14496_3                             = 0x40
type instance FromEnum ObjectTypeIndication 'VisualIso13818_2_SimpleProfile              = 0x60
type instance FromEnum ObjectTypeIndication 'VisualIso13818_2_MainProfile                = 0x61
type instance FromEnum ObjectTypeIndication 'VisualIso13818_2_SnrProfile                 = 0x62
type instance FromEnum ObjectTypeIndication 'VisualIso13818_2_SpatialProfile             = 0x63
type instance FromEnum ObjectTypeIndication 'VisualIso13818_2_HighProfile                = 0x64
type instance FromEnum ObjectTypeIndication 'VisualIso13818_2_422Profile                 = 0x65
type instance FromEnum ObjectTypeIndication 'AudioIso13818_7_MainProfile                 = 0x66
type instance FromEnum ObjectTypeIndication 'AudioIso13818_7_LowComplexityProfile        = 0x67
type instance FromEnum ObjectTypeIndication 'AudioIso13818_7_ScalableSamplingRateProfile = 0x68
type instance FromEnum ObjectTypeIndication 'AudioIso13818_3                             = 0x69
type instance FromEnum ObjectTypeIndication 'VisualIso11172_2                            = 0x6A
type instance FromEnum ObjectTypeIndication 'AudioIso11172_3                             = 0x6B
type instance FromEnum ObjectTypeIndication 'VisualIso10918_1                            = 0x6C
type instance FromEnum ObjectTypeIndication 'VisualIso15444_1                            = 0x6E
type instance FromEnum ObjectTypeIndication 'NoObjectTypeSpecified                       = 0xFF

type instance EnumFieldSize ObjectTypeIndication = 8

-- * Stream Type

type family
  GetStreamType (t :: k) :: StreamType

data StreamType =
    ObjectDescriptorStream
  | ClockReferenceStream
  | SceneDescriptionStream_Iso14496_11
  | VisualStream
  | AudioStream
  | Mpeg7Stream
  | IpmpStream
  | ObjectContentInfoStream
  | MpegJStream
  | InteractionStream
  | IpmpToolStream_Iso14496_13

type instance FromEnum StreamType 'ObjectDescriptorStream             = 1
type instance FromEnum StreamType 'ClockReferenceStream               = 2
type instance FromEnum StreamType 'SceneDescriptionStream_Iso14496_11 = 3
type instance FromEnum StreamType 'VisualStream                       = 4
type instance FromEnum StreamType 'AudioStream                        = 5
type instance FromEnum StreamType 'Mpeg7Stream                        = 6
type instance FromEnum StreamType 'IpmpStream                         = 7
type instance FromEnum StreamType 'ObjectContentInfoStream            = 8
type instance FromEnum StreamType 'MpegJStream                        = 9
type instance FromEnum StreamType 'InteractionStream                  = 0xa
type instance FromEnum StreamType 'IpmpToolStream_Iso14496_13         = 0xb

type instance EnumFieldSize StreamType = 6

data ProfileLevelIndicationIndexDescriptor =
   ProfileIndex (Maybe Nat)

type family
  ToBitRecordProfileIndex (x :: ProfileLevelIndicationIndexDescriptor) :: BitRecordField where
  ToBitRecordProfileIndex ('ProfileIndex ('Just n)) = FieldU8 := n
  ToBitRecordProfileIndex ('ProfileIndex 'Nothing) = "profileLevelIndicationIndex" :=> FieldU8

type instance
  ToBitRecord (x :: ProfileLevelIndicationIndexDescriptor) =
  BaseDescriptor
    'ProfileLevelIndicationIndexDescr
    (ToBitRecord (ToBitRecordProfileIndex x))
