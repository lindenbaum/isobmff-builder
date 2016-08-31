-- | @mp4a@ Audio sample entry according to ISO 14496-14
module Data.ByteString.Mp4.Boxes.Mp4AudioSampleEntry where

import Data.ByteString.IsoBaseFileFormat.Box
import Data.ByteString.IsoBaseFileFormat.Boxes
import Data.ByteString.IsoBaseFileFormat.Util.FullBox
import Data.ByteString.IsoBaseFileFormat.ReExports
import Data.ByteString.Mp4.Boxes.ElementaryStreamDescriptor
import Data.ByteString.Mp4.Boxes.BaseDescriptor


-- | A /body/ for 'AudioSampleEntry'. This 'IsBoxContent' with an
-- 'ElementaryStreamDescriptor' for ISO-14496-3 audio, with audio decoder
-- specific info.
newtype Mp4AudioSampleEntry





-- | Create an 'StaticMp4AudioSampleEntry'
audioSampleEntry
  :: AudioSampleEntry ()
  -> Box AudioEsd
  -> AudioSampleEntry (Box AudioEsd)
audioSampleEntry ase eds = const eds <$> ase

-- | Create an mp4 audio elementary stream descriptor full box
audioEsd
  :: Box AudioEsd
audioEsd = fullBox 0 $ AudioEsdContent

-- | Consists of 'ElementaryStreamDescriptor's
newtype AudioEsdContent =
  AudioEsdContent (BaseDescriptor ESDescriptorAudio)
  deriving IsBoxContent

instance Default StaticMp4AudioEsdContent where
    def = StaticMp4AudioEsdContent $ staticESDescriptorAudio 1 1

type AudioEsd = FullBox StaticMp4AudioEsdContent 0

instance IsBox StaticMp4AudioEsdContent
type instance BoxTypeSymbol StaticMp4AudioEsdContent = "mp4a"

instance IsBox (AudioSampleEntry (Box StaticMp4AudioEsd))


-- * Interface from ISO 14496-3 (Audio)

data AudioObjectType where
  AudioObjectType :: Nat -> AudioObjectType

type instance ToBitRecord ('AudioObjectType n) =
  'ReplacePretty
    (If (n <=? 30) ("AudioObjectType") ("ExtAudioObjectType") <:> PutHex8 n)
    (AudioObjectTypeField1 n :>: AudioObjectTypeField2 n)

type family AudioObjectTypeField1 (n :: Nat) :: BitRecordField where
  AudioObjectTypeField1 n = If (n <=? 30) (Field 5 := n) (Field 5 := 31)

type family AudioObjectTypeField2 (n :: Nat) :: BitRecord where
  AudioObjectTypeField2 n = If (n <=? 30) 'EmptyBitRecord (ToBitRecord (Field 6 := (n - 31)))
