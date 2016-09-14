{-# LANGUAGE UndecidableInstances #-}
-- | @mp4a@ Audio sample entry according to ISO 14496-14
module Data.ByteString.Mp4.Boxes.Mp4AudioSampleEntry where

import           Data.ByteString.IsoBaseFileFormat.Box
import           Data.ByteString.IsoBaseFileFormat.Boxes
import           Data.ByteString.IsoBaseFileFormat.Util.BoxFields
import           Data.ByteString.IsoBaseFileFormat.ReExports
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
  :: AudioSampleEntry ()
  -> AudioEsd
  -> AudioSampleEntry (Box AudioEsd)
audioSampleEntry ase eds = const (Box eds) <$> ase

-- | Create an mp4 audio elementary stream descriptor full box
aacLcMono16kEsd :: EnumValue SamplingFreqTable -> EnumValue ChannelConfigTable -> AudioEsd
aacLcMono16kEsd sf cc = AudioEsd (esdBox (Proxy @Mp4AacLcEsDescriptor) False 0 0 0 sf cc)

-- | Consists of an 'ElementaryStreamDescriptor' derived from a 'DecoderSpecificInfo'.
newtype AudioEsd =
  AudioEsd EsdBox
  deriving (IsBoxContent)

instance IsBox AudioEsd
type instance BoxTypeSymbol AudioEsd = "mp4a"

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
