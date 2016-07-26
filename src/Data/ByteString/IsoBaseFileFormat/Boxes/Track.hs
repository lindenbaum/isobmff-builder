-- | Meta data for a presentation of a /movie/.
module Data.ByteString.IsoBaseFileFormat.Boxes.Track where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box

-- * @trak@ Box
-- | Compose a 'Track' box from the given boxes.
track
  :: (ValidContainerBox brand Track ts)
  => Boxes brand ts -> Box brand Track
track = containerBox

-- | Container box for tracks.
data Track

instance IsBoxType Track where
  toBoxType _ _ = StdType "trak"
