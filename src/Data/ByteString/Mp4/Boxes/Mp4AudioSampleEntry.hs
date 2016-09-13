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
-- audioEsd
--  :: Tagged "esId" Word16 -> Tagged "streamPriority" Word64 -> AudioEsd
-- audioEsd = runHoley $ hoistR AudioEsd $ bitBoxHoley $ Proxy @ESDescriptorSimple

-- | Consists of an 'ElementaryStreamDescriptor' derived from a 'DecoderSpecificInfo'.
newtype AudioEsd =
  AudioEsd (U8 "TODO")
  deriving (Default, IsBoxContent)

instance IsBox AudioEsd
type instance BoxTypeSymbol AudioEsd = "mp4a"

type Mp4AacLcEsDescriptor  =
  ESDescriptorMp4File DefaultEsId Mp4AacLcAudioDecoderConfigDescriptor

-- xxx = bitStringPrinter (Proxy @(Eval (BitRecordOfDescriptor $~ Eval Mp4AacLcEsDescriptor))
-- :kind! (Eval (BitRecordOfDescriptor $~ Eval Mp4AacLcEsDescriptor))
-- putStrLn $ showRecord (Proxy @(Eval (BitRecordOfDescriptor $~ Eval Mp4AacLcEsDescriptor)))
-- bitStringPrinter (Proxy @(Eval (BitRecordOfDescriptor $~ Eval Mp4AacLcEsDescriptor)))


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

-- :kind! (Eval (BitRecordOfDescriptor $~ (Eval (Mp4AacLcAudioDecoderConfigDescriptor 'SF44100 'SingleChannel)))
-- putStrLn $ showARecord (Proxy @(BitRecordOfDescriptor $~ (Eval (Mp4AacLcAudioDecoderConfigDescriptor 'SF44100 'SingleChannel))))
-- bitStringPrinter (Proxy @(Eval (BitRecordOfDescriptor $~ (Eval (Mp4AacLcAudioDecoderConfigDescriptor 'SF44100 'SingleChannel)))))
-- bitStringPrinter (Proxy @(Eval (BitRecordOfDescriptor $~ Eval  (Mp4AacLcAudioDecoderConfigDescriptor 'SF44100 'SingleChannel)))) True 1 2 3
