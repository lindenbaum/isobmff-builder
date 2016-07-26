-- | Meta data for a presentation of a /movie/.
module Data.ByteString.IsoBaseFileFormat.Boxes.Movie where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box

-- | Compose a set of boxes into a 'Movie'
movie :: (ValidContainerBox brand Movie ts)
      => Boxes brand ts -> Box brand Movie
movie = containerBox

-- | The metadata for a presentation, a single 'Movie' which occurs only once
-- and top-level. It is pretty empty on it's own, but it contains nested boxes
-- with all the relevant meta data.
data Movie

instance IsBoxType Movie where
  toBoxType _ _ = StdType "moov"
