-- | Meta data for a presentation of a /movie/.
module Data.ByteString.IsoBaseFileFormat.Boxes.Movie where

import Data.ByteString.IsoBaseFileFormat.Box

-- | Compose a set of boxes into a 'Movie'
movie :: Boxes ts -> Box (ContainerBox Movie ts)
movie = containerBox ()

-- | The metadata for a presentation, a single 'Movie' which occurs only once
-- and top-level. It is pretty empty on it's own, but it contains nested boxes
-- with all the relevant meta data.
data Movie

instance IsBox Movie where
  type BoxContent Movie = ()

type instance BoxTypeSymbol Movie = "moov"
