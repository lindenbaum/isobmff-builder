-- | @mp4a@ Audio sample entry according to ISO 14496-14
module Data.ByteString.Mp4.Boxes.Mp4AudioSampleEntry where

import Data.ByteString.IsoBaseFileFormat.Box
import Data.ByteString.IsoBaseFileFormat.Boxes
import Data.ByteString.IsoBaseFileFormat.Util.FullBox
import Data.ByteString.IsoBaseFileFormat.ReExports
import Data.ByteString.Mp4.Boxes.ElementaryStreamDescriptor

-- | Create an 'Mp4AudioSampleEntry'
mp4AudioSampleEntry
  :: AudioSampleEntry ()
  -> Box Mp4AudioEsd
  -> Mp4AudioSampleEntry
mp4AudioSampleEntry ase eds = const eds <$> ase

-- | The MPEG-4 Audio sample entry
type Mp4AudioSampleEntry = AudioSampleEntry (Box Mp4AudioEsd)

-- | Create an mp4 audio elementary stream descriptor full box
mp4Audio
  :: ElementaryStreamDescriptor -> Box Mp4AudioEsd
mp4Audio = fullBox 0 . Mp4AudioEsdContent

-- | Consists of 'ElementaryStreamDescriptor's
newtype Mp4AudioEsdContent =
  Mp4AudioEsdContent ElementaryStreamDescriptor
  deriving (IsBoxContent, Default)

type Mp4AudioEsd = FullBox Mp4AudioEsdContent 0

instance IsBox Mp4AudioEsdContent
type instance BoxTypeSymbol Mp4AudioEsdContent = "mp4a"

instance IsBox (AudioSampleEntry (Box Mp4AudioEsd))
