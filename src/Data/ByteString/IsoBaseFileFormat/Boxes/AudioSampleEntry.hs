-- | Detailed audio sample description.
module Data.ByteString.IsoBaseFileFormat.Boxes.AudioSampleEntry where

import Data.ByteString.IsoBaseFileFormat.Box
import Data.ByteString.IsoBaseFileFormat.Util.BoxFields
import Data.ByteString.IsoBaseFileFormat.Boxes.Handler
import Data.ByteString.IsoBaseFileFormat.ReExports

-- | Fields of audio sample entries
newtype AudioSampleEntry b where
    AudioSampleEntry
      :: Constant (U32Arr "reserved" 2) '[0,0]
      :+ Template (U16 "channelcount") 2
      :+ Template (U16 "samplesize") 16
      :+ U16 "pre_defined"
      :+ Constant (U16 "reserved") 0
      :+ Template (U32 "samplerate")
         (DefaultSoundSamplerate * 65536) -- TODO implement fix point integer
      :+ b
      -> AudioSampleEntry b
      deriving (Default, IsBoxContent)

instance Functor AudioSampleEntry where
  fmap fun (AudioSampleEntry (a :+ b :+ c :+ d :+ e :+ f :+ x)) =
    AudioSampleEntry (a :+ b :+ c :+ d :+ e :+ f :+ fun x)

type DefaultSoundSamplerate = 48000

type instance GetHandlerType (AudioSampleEntry b) = 'AudioTrack
type instance BoxTypeSymbol (AudioSampleEntry c) = BoxTypeSymbol c
