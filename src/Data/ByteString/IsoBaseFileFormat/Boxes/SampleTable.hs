-- | Time and indexing information for the media samples in a track,a sample
-- table can reference 'DataReference' boxes via a 'SampleTableDescription'.
--
-- **NOTE** A /sample/ referes to a **frame** e.g. a video frame frame or a
-- block of audio samples.
module Data.ByteString.IsoBaseFileFormat.Boxes.SampleTable where

import Data.ByteString.IsoBaseFileFormat.Box

-- | A sample table contains no fieds.
data SampleTable = SampleTable

-- | Create a hint media header data box.
sampleTable :: Boxes ts -> Box (ContainerBox SampleTable ts)
sampleTable = containerBox ()

instance IsBox SampleTable where
  type BoxContent SampleTable = ()
type instance BoxTypeSymbol SampleTable = "stbl"
