-- | Detailed information about the coced, as well as coding specific
-- initialization. Depending on the 'Trak' media type different kinds of sample
-- entry boxes are entailed.
module Data.ByteString.IsoBaseFileFormat.Boxes.SampleDescription where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box
import Data.ByteString.IsoBaseFileFormat.Boxes.FullBox
import Data.ByteString.IsoBaseFileFormat.Boxes.SampleEntry
import Data.ByteString.IsoBaseFileFormat.Boxes.BoxFields
import Data.Singletons.Prelude.List (Length)

-- | A sample table contains no fieds.
newtype SampleDescription handlerType =
  SampleDescription (U32 "entry_count")
  deriving (Default,IsBoxContent)

-- | Create a hint media header data box.
sampleDescription
  :: (KnownNat (Length ts))
  => Boxes ts
  -> Box (ContainerBox (FullBox (SampleDescription h) 0) ts)
sampleDescription bs =
  containerBox (FullBox 0 $ SampleDescription (typeListLength bs))
               bs

instance IsBox (SampleDescription h)

type instance BoxTypeSymbol (SampleDescription  h) = "stsd"
