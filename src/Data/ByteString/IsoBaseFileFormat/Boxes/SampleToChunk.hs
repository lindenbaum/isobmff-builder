-- | The chunks into which samples are grouped, can vary in size, as can the
-- samples within a chunk. This compact table contains the info to find the
-- chunk and offset within of a sample.
module Data.ByteString.IsoBaseFileFormat.Boxes.SampleToChunk where

import Data.ByteString.IsoBaseFileFormat.Box
import Data.ByteString.IsoBaseFileFormat.Util.FullBox
import Data.ByteString.IsoBaseFileFormat.Util.BoxFields

-- | An alias for the box content type.
type SampleToChunk = FullBox SampleToChunkTable 0

-- | Define an entry of the compact table. An entry represents a group of chunks
-- of the same size and the same 'SampleDescription'. The @first_chunk@ is an
-- index of the first chunk in a sequence of chunks correspondiing to an entry.
-- The very first  entry has an index of 1.
type StscEntry =
  U32 "first_chunk" :+ U32 "samples_per_chunk" :+ U32 "sample_description_index"

-- | A compact table to map decoding time to sample number.
newtype SampleToChunkTable =
  SampleToChunkTable (ListContent (U32 "entry_count") StscEntry)
  deriving (Default, IsBoxContent)

-- | Create a hint media header data box.
sampleToChunk :: [StscEntry] -> Box SampleToChunk
sampleToChunk = fullBox 0 . SampleToChunkTable . ListContent

instance IsBox SampleToChunkTable

type instance BoxTypeSymbol SampleToChunkTable = "stsc"
