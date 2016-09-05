{-# LANGUAGE UndecidableInstances #-}
-- | @mp4a@ Audio sample entry according to ISO 14496-14
module Data.ByteString.Mp4.Boxes.Mp4AudioSampleEntry where

import           Data.ByteString.IsoBaseFileFormat.Box
import           Data.ByteString.IsoBaseFileFormat.Boxes
import           Data.ByteString.IsoBaseFileFormat.Util.BoxFields
import           Data.ByteString.IsoBaseFileFormat.ReExports
import           Data.ByteString.Mp4.Boxes.BaseDescriptor
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
  :: forall (di :: IsA (DecoderSpecificInfo 'AudioIso14496_3 'AudioStream)) .
    AudioSampleEntry ()
  -> AudioEsd di
  -> AudioSampleEntry (Box (AudioEsd di))
audioSampleEntry ase eds = const (Box eds) <$> ase

-- | Create an mp4 audio elementary stream descriptor full box
-- audioEsd
--  :: Tagged "esId" Word16 -> Tagged "streamPriority" Word64 -> AudioEsd
-- audioEsd = runHoley $ hoistR AudioEsd $ bitBoxHoley $ Proxy @ESDescriptorSimple

-- | Consists of an 'ElementaryStreamDescriptor' derived from a 'DecoderSpecificInfo'.
newtype AudioEsd (di :: IsA (DecoderSpecificInfo 'AudioIso14496_3 'AudioStream)) =
  AudioEsd (BitBox (Mp4AudioEsDescriptor di))

type Mp4AudioEsDescriptor di =
  (ESDescriptorMp4File
    (DecoderConfigDescriptor
     'AudioIso14496_3
     'AudioStream
     '[Eval di]
     '[]))

deriving instance KnownNat (BitRecordSize (ToBitRecord (Mp4AudioEsDescriptor di)))
  => IsBoxContent (AudioEsd di)

-- instance KnownNat (BitRecordSize (ToBitRecord (Mp4AudioEsDescriptor di))) => IsBox (AudioEsd di)
-- type instance BoxTypeSymbol (AudioEsd x) = "mp4a"

-- -- TODO rename project

-- -- TODO seperate this and other modules so theres the same seperation as in between
-- -- the parts of the standard.
-- esDescriptorBitBox
--   :: forall (di :: IsA (Descriptor 'DecoderConfigDescr)) .
--     (BitStringBuilderHoley (Proxy (ESDescriptorMp4File di)) (BitBox (ESDescriptorMp4File di)))
--   => Proxy di
--   -> ToBitStringBuilder (Proxy (ESDescriptorMp4File di)) (BitBox (ESDescriptorMp4File di))
-- esDescriptorBitBox _ =
--   bitBoxWithArgs (Proxy @(ESDescriptorMp4File di))

type Mp4AacLcAudioDecoderConfigDescriptor =
  DecoderConfigDescriptor
   'AudioIso14496_3
   'AudioStream
  '[Eval (NonSbrAudioConfig
          (GASpecificConfig 'AacLc 'False 'Nothing 'Nothing)
          (SetEnum SamplingFreq 'SF88200)
          (SetEnum ChannelConfig 'GasChannelConfig))]
  '[]


{-
 :kind! (Eval (NonSbrAudioConfig
         (GASpecificConfig 'AacLc 'False 'Nothing 'Nothing)
         ('MkEnumOf ('BitRecordMember ('AssignF 1 ('MkField Word64 4))))
         ('MkEnumOf ('BitRecordMember ('AssignF 1 ('MkField Word64 4))))))

bitStringPrinter (Proxy @(ToBitRecord (ESDescriptor 'False 'False 'False Mp4AacLcAudioDecoderConfigDescriptor Mp4SyncLayerDescriptor)))

bitStringPrinter (Proxy @(ToBitRecord (ESDescriptor 'False 'False 'False Mp4AacLcAudioDecoderConfigDescriptor Mp4SyncLayerDescriptor))) 3 3
-}
