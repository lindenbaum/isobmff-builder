{-# LANGUAGE UndecidableInstances #-}
-- | @mp4a@ Audio sample entry according to ISO 14496-14
module Data.ByteString.Mp4.Boxes.Mp4AudioSampleEntry where

import           Data.ByteString.IsoBaseFileFormat.Box
import           Data.ByteString.IsoBaseFileFormat.Boxes
import           Data.ByteString.IsoBaseFileFormat.ReExports
import           Data.ByteString.IsoBaseFileFormat.Util.BoxFields
import           Data.ByteString.Mp4.Boxes.ElementaryStreamDescriptor
import           Data.ByteString.Mp4.Boxes.DecoderSpecificInfo
import           Data.ByteString.Mp4.Boxes.DecoderConfigDescriptor
import           Data.ByteString.Mp4.Boxes.AudioSpecificConfig

-- | A /body/ for 'AudioSampleEntry'. This 'IsBoxContent' with an
-- 'ElementaryStreamDescriptor' for ISO-14496-3 audio, with audio decoder
-- specific info.

-- | Create an 'AudioSampleEntry' with an 'AudioEsd'
--
-- TODO generalize this, allow all parameters,e.g. also for SBR though the
audioSampleEntry
  :: U16 "data_reference_index"
  -> AudioSampleEntry AudioEsd
  -> Box (SampleEntry (AudioSampleEntry AudioEsd))
audioSampleEntry drefIndex ase =
  sampleEntry drefIndex ase

type instance BoxTypeSymbol AudioEsd = "mp4a"

-- | Create an mp4 audio elementary stream descriptor full box
aacLcAudioSampleEntry
  :: SamplingFreqTable
  ->ChannelConfigTable
  -> U16 "samplesize"
  -> AudioSampleEntry AudioEsd
aacLcAudioSampleEntry sf cc sampleSize =
  AudioSampleEntry
    (Constant
    :+ Custom (Scalar (channelConfigToNumber cc))
    :+ Custom sampleSize
    :+ 0
    :+ Constant
    :+ Custom (Scalar (sampleRateToNumber sf * 65536))
    :+ (AudioEsd
         (esdBox
          (Proxy @Mp4AacLcEsDescriptor) False 0 0 0
          (sampleRateToEnum sf) (channelConfigToEnum cc))))

-- | Consists of an 'ElementaryStreamDescriptor' derived from a 'DecoderSpecificInfo'.
newtype AudioEsd =
  AudioEsd EsdBox
  deriving (IsBoxContent)

type Mp4AacLcEsDescriptor  =
  ESDescriptorMp4File DefaultEsId Mp4AacLcAudioDecoderConfigDescriptor

type Mp4AacLcAudioDecoderConfigDescriptor  =
  DecoderConfigDescriptor
  'AudioIso14496_3
  'AudioStream
  '[NonSbrAudioConfig
     'AacLc
     DefaultGASpecificConfig
     (EnumParam "samplingFreq" SamplingFreq)
     (EnumParam "channelConfig" ChannelConfig)]
  '[]
