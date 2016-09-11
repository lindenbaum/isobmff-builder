{-# LANGUAGE UndecidableInstances #-}
module Data.ByteString.Mp4.Boxes.DecoderSpecificInfo where

import           Data.ByteString.Mp4.Boxes.BaseDescriptor
import           Data.Type.BitRecords
import           Data.Type.Pretty
import           Data.Kind.Extra
import           Data.Kind (type Type)

-- * Abstract class for opaque object- and stream type dependent decoder
-- settings.

-- | Base type of decoders
data DecoderSpecificInfo :: ObjectTypeIndication -> StreamType -> Type where
  MkDecoderSpecificInfo :: IsA BitRecord -> DecoderSpecificInfo o s

data DescriptorOfDecoderSpecificInfo
  :: IsA (DecoderSpecificInfo ot st :-> Descriptor 'DecSpecificInfo)

type instance DescriptorOfDecoderSpecificInfo $~ 'MkDecoderSpecificInfo body =
   'MkDescriptor (PutStr "decoder-specific-info" #$ body)

type ObjectTypeIndicationEnum = FixedEnum ObjectTypeIndication 8

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

-- * Stream Type

type family
  GetStreamType (t :: k) :: StreamType

type StreamTypeEnum = FixedEnum StreamType 6

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
