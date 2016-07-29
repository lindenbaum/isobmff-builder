-- | Time and indexing information for the media samples in a track,a sample
-- table can reference 'DataReference' boxes via a 'SampleTableDescription'.
module Data.ByteString.IsoBaseFileFormat.Boxes.SampleTable where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box

-- | A sample table contains no fieds.
data SampleTable = SampleTable

-- | Create a hint media header data box.
sampleTable :: Boxes ts -> Box (ContainerBox SampleTable ts)
sampleTable = containerBox ()

instance IsBox SampleTable where
  type BoxContent SampleTable = ()
type instance BoxTypeSymbol SampleTable = "stbl"
