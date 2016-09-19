-- | Meta data for a presentation of a /movie fragment/.
module Data.ByteString.IsoBaseFileFormat.Boxes.MovieFragmentHeader where

import Data.ByteString.IsoBaseFileFormat.Box
import Data.ByteString.IsoBaseFileFormat.Util.BoxFields
import Data.ByteString.IsoBaseFileFormat.Util.FullBox

-- * @mvhd@ Box

-- | Construct a 'MovieFragmentHeader' box.
movieFragmentHeader
  :: MovieFragmentHeader-> Box (FullBox MovieFragmentHeader 0)
movieFragmentHeader = fullBox 0

-- | Movie fragment meta data
-- The sequence number, just an increasing number, of this fragment
newtype MovieFragmentHeader where
        MovieFragmentHeader ::  U32 "sequence_number" -> MovieFragmentHeader
    deriving (IsBoxContent)

instance IsBox MovieFragmentHeader

type instance BoxTypeSymbol MovieFragmentHeader = "mfhd"
