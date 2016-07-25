module Data.ByteString.IsoBaseFileFormat.Boxes.MediaData where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box
import qualified Data.ByteString as B

-- | Media data box
data MediaData

instance IsBoxType' MediaData where
  -- | Contents of a 'mdat' box are just bytes from a strict 'ByteString'
  type BoxContent MediaData = B.ByteString
  toBoxType' _ = StdType "mdat"

-- | Create a 'MediaDataBox' from a strict 'ByteString'
mediaDataBox :: ValidBox b MediaData => B.ByteString -> Box b MediaData
mediaDataBox = closedBox
