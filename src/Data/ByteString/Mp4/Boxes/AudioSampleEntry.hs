-- | @mp4a@ Audio sample entry according to ISO 14496-14
module Data.ByteString.Mp4.Boxes.AudioSampleEntry where

import Data.ByteString.IsoBaseFileFormat.Box
import Data.ByteString.IsoBaseFileFormat.Util.FullBox
import Data.ByteString.IsoBaseFileFormat.ReExports
import Data.ByteString.Mp4.Boxes.ElementaryStreamDescriptor

-- | The MPEG-4 Audio sample entry
type Mp4AudioSampleEntry = FullBox ElementaryStreamDescriptor 0

-- | Consists of 'ElementaryStreamDescriptor's
newtype Mp4AudioEsd =
  Mp4AudioEsd ElementaryStreamDescriptor
  deriving (IsBoxContent, Default)

type instance BoxTypeSymbol Mp4AudioEsd = "mp4a"

instance IsBox Mp4AudioEsd
