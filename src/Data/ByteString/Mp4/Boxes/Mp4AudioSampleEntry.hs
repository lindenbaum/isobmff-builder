{-# LANGUAGE UndecidableInstances #-}
-- | @mp4a@ Audio sample entry according to ISO 14496-14
module Data.ByteString.Mp4.Boxes.Mp4AudioSampleEntry where

import           Data.ByteString.IsoBaseFileFormat.Box
import           Data.ByteString.IsoBaseFileFormat.Boxes
import           Data.ByteString.IsoBaseFileFormat.Util.BoxFields
import           Data.ByteString.IsoBaseFileFormat.ReExports
import           Data.ByteString.Mp4.Boxes.ElementaryStreamDescriptor

import           Data.Type.BitRecords
import           Data.Type.Pretty


-- | A /body/ for 'AudioSampleEntry'. This 'IsBoxContent' with an
-- 'ElementaryStreamDescriptor' for ISO-14496-3 audio, with audio decoder
-- specific info.

-- | Create an 'AudioSampleEntry' with an 'AudioEsd'
audioSampleEntry
  :: AudioSampleEntry ()
  -> AudioEsd
  -> AudioSampleEntry (Box AudioEsd)
audioSampleEntry ase eds = const (Box eds) <$> ase

-- | Create an mp4 audio elementary stream descriptor full box
audioEsd
  :: Tagged "esId" Word16 -> Tagged "streamPriority" Word64 -> AudioEsd
audioEsd = runHoley $ hoistR AudioEsd $ bitBoxHoley $ Proxy @ESDescriptorSimple

-- | Consists of 'ElementaryStreamDescriptor's
newtype AudioEsd =
  AudioEsd (BitBox ESDescriptorSimple)
  deriving IsBoxContent

instance Default AudioEsd where
    def = audioEsd def def

instance IsBox AudioEsd
type instance BoxTypeSymbol AudioEsd = "mp4a"


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
