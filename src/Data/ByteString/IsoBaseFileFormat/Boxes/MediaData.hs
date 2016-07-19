module Data.ByteString.IsoBaseFileFormat.Boxes.MediaData where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box
import qualified Data.ByteString as B

-- | Media data box
type MediaDataBox = Box MediaData

-- | Create a 'MediaDataBox' from a strict 'ByteString'
mdatBox :: B.ByteString -> MediaDataBox
mdatBox = box "mdat" . MediaData

-- | Contents of a 'mdat' box are just bytes from a strict 'ByteString'
newtype MediaData =
  MediaData B.ByteString

instance IsBoxContent MediaData where
  boxSize (MediaData bs) = fromIntegral $ B.length bs
  boxBuilder (MediaData bs) = byteString bs
