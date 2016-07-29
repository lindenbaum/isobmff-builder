-- | Detailed audio sample description.
module Data.ByteString.IsoBaseFileFormat.Boxes.AudioSampleEntry where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box
import Data.ByteString.IsoBaseFileFormat.Boxes.BoxFields
import Data.ByteString.IsoBaseFileFormat.Boxes.Handler
import Data.ByteString.IsoBaseFileFormat.Boxes.SampleEntry

-- | Construct an audio sample entry box.
audioSampleEntry ::
     (KnownSymbol
       (BoxTypeSymbol (SampleEntry 'AudioTrack (AudioCoding c))))
  => AudioCoding c
  -> U16 "data_reference_index"
  -> SampleEntry 'AudioTrack (AudioCoding c)
  -> Box (SampleEntry 'AudioTrack (AudioCoding c))
audioSampleEntry _ = sampleEntry

-- | Fields if audio sample entries
newtype instance SampleEntry 'AudioTrack (AudioCoding c) where
    AudioSampleEntry
      :: Constant (U32Arr "reserved" 2) '[0,0]
      :+ Template (U16 "channelcount") 2
      :+ Template (U16 "samplesize") 16
      :+ U16 "pre_defined"
      :+ Constant (U16 "reserved") 0
      :+ Template (U32 "samplerate") (DefaultSoundSamplerate * 65536)
      -- TODO implement fix point integer
      -> SampleEntry 'AudioTrack (AudioCoding c)
    deriving (IsBoxContent, Default)

type DefaultSoundSamplerate = 48000

-- | A coproduct of audio codec types
data family AudioCoding (c :: Symbol)

-- | The MPEG-4 AAC Audio codec
data instance AudioCoding "mp4a" = Mpeg4Aac

type instance BoxTypeSymbol (AudioCoding c) = c
