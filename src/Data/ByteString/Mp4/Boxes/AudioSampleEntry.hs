-- | @mp4a@ Audio sample entry according to ISO 14496-14
module Data.ByteString.Mp4.Boxes.AudioSampleEntry where

import Data.ByteString.IsoBaseFileFormat.Box
import Data.ByteString.IsoBaseFileFormat.Util.FullBox
import Data.ByteString.IsoBaseFileFormat.ReExports
import Data.ByteString.Mp4.Boxes.ElementaryStreamDescriptor

-- | The MPEG-4 AAC Audio codec
newtype Mp4AudioSampleEntry =
  Mp4AudioSampleEntry (FullBox ElementaryStreamDescriptor 0)

type ElementaryStreamDescriptor = Tagged "TODO" Word32

type instance BoxTypeSymbol Mp4AudioSampleEntry = "mp4a"

instance IsBox Mp4AudioSampleEntry
