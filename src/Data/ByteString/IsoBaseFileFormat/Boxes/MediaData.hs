-- | Media data box
module Data.ByteString.IsoBaseFileFormat.Boxes.MediaData where

import Data.ByteString.IsoBaseFileFormat.Box
import qualified Data.ByteString as B

-- | Media data box phantom type
newtype MediaData = MediaData B.ByteString
  deriving (Show, IsBoxContent)

instance IsBox MediaData where
  -- | Contents of a 'mdat' box are just bytes from a strict 'ByteString'
  type BoxContent MediaData = MediaData

type instance BoxTypeSymbol MediaData = "mdat"

-- | Create a 'MediaDataBox' from a strict 'ByteString'
mediaData :: MediaData -> Box MediaData
mediaData = Box
