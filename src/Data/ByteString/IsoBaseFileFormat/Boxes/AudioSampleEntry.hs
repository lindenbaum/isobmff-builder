-- | Detailed audio sample description.
module Data.ByteString.IsoBaseFileFormat.Boxes.AudioSampleEntry where

import Data.ByteString.IsoBaseFileFormat.Box
import Data.ByteString.IsoBaseFileFormat.Util.BoxFields
import Data.ByteString.IsoBaseFileFormat.Boxes.Handler
import Data.ByteString.IsoBaseFileFormat.Boxes.SampleEntry
import Data.ByteString.IsoBaseFileFormat.ReExports


-- | Construct an audio sample entry box.
audioSampleEntry ::
     Box c
  -> U16 "data_reference_index"
  -> SampleEntry 'AudioTrack (Box c)
  -> Box (SampleEntry 'AudioTrack (Box c))
audioSampleEntry _ = sampleEntry

-- | Fields if audio sample entries
newtype instance SampleEntry 'AudioTrack (Box c) where
    AudioSampleEntry
      :: Constant (U32Arr "reserved" 2) '[0,0]
      :+ Template (U16 "channelcount") 2
      :+ Template (U16 "samplesize") 16
      :+ U16 "pre_defined"
      :+ Constant (U16 "reserved") 0
      :+ Template (U32 "samplerate") (DefaultSoundSamplerate * 65536) -- TODO implement fix point integer
      :+ Tagged "ESDescriptor" (Box c)
      -> SampleEntry 'AudioTrack (Box c)
    deriving (IsBoxContent)

deriving instance (Default (BoxContent c)) => Default (SampleEntry 'AudioTrack (Box c))

type DefaultSoundSamplerate = 48000
