-- | A table assigns each video frame or audio sample block, this is useful for
-- seeking in complex media files.
module Data.ByteString.IsoBaseFileFormat.Boxes.TimeToSample where

import Data.ByteString.IsoBaseFileFormat.Box
import Data.ByteString.IsoBaseFileFormat.Util.FullBox
import Data.ByteString.IsoBaseFileFormat.Util.BoxFields

-- | An alias for the box content type.
type TimeToSample = FullBox TimeToSampleTable 0

-- | Define an entry of the compact table, that specifes the namber of samples
--  with the same time delta. (All this to help random seeking in media).
type SttsEntry = U32 "sample_count" :+ U32 "sample_delta"

-- | A compact table to map decoding time to sample number.
newtype TimeToSampleTable =
  TimeToSampleTable (ListContent (U32 "entry_count") SttsEntry)
  deriving (Default, IsBoxContent)

-- | Create a hint media header data box.
timeToSample :: [SttsEntry] -> Box TimeToSample
timeToSample = fullBox 0 . TimeToSampleTable . ListContent

instance IsBox TimeToSampleTable

type instance BoxTypeSymbol TimeToSampleTable = "stts"
