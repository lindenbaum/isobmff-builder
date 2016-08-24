-- | @mp4a@ Audio sample entry according to ISO 14496-14
module Data.ByteString.Mp4.Boxes.Mp4AudioSampleEntry where

import Data.ByteString.IsoBaseFileFormat.Box
import Data.ByteString.IsoBaseFileFormat.Boxes
import Data.ByteString.IsoBaseFileFormat.Util.FullBox
import Data.ByteString.IsoBaseFileFormat.ReExports
import Data.ByteString.Mp4.Boxes.ElementaryStreamDescriptor

-- | Create an 'StaticMp4AudioSampleEntry'
staticMp4AudioSampleEntry
  :: AudioSampleEntry ()
  -> Box StaticMp4AudioEsd
  -> StaticMp4AudioSampleEntry
staticMp4AudioSampleEntry ase eds = const eds <$> ase

-- | The MPEG-4 Audio sample entry
type StaticMp4AudioSampleEntry = AudioSampleEntry (Box StaticMp4AudioEsd)

-- | Create an mp4 audio elementary stream descriptor full box
staticStaticMp4Audio
  :: StaticESDescriptorAudio -> Box StaticMp4AudioEsd
staticStaticMp4Audio = fullBox 0 . StaticMp4AudioEsdContent

-- | Consists of 'ElementaryStreamDescriptor's
newtype StaticMp4AudioEsdContent = StaticMp4AudioEsdContent StaticESDescriptorAudio
  deriving IsBoxContent

instance Default StaticMp4AudioEsdContent where
    def = StaticMp4AudioEsdContent $ staticESDescriptorAudio 1 (Tagged False) (Tagged False) 1

type StaticMp4AudioEsd = FullBox StaticMp4AudioEsdContent 0

instance IsBox StaticMp4AudioEsdContent
type instance BoxTypeSymbol StaticMp4AudioEsdContent = "mp4a"

instance IsBox (AudioSampleEntry (Box StaticMp4AudioEsd))
