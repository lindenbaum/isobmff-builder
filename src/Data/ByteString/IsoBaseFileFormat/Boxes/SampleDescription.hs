-- | Detailed information about the coced, as well as coding specific
-- initialization. Depending on the 'Trak' media type different kinds of sample
-- entry boxes are entailed.
module Data.ByteString.IsoBaseFileFormat.Boxes.SampleDescription where

import Data.ByteString.IsoBaseFileFormat.Box
import Data.ByteString.IsoBaseFileFormat.Util.FullBox
import Data.ByteString.IsoBaseFileFormat.Util.BoxFields
import Data.Singletons.Prelude.List (Length)
import Data.ByteString.IsoBaseFileFormat.ReExports

-- | A sample table contains no fieds.
newtype SampleDescription =
  SampleDescription (U32 "entry_count")
  deriving (Default,IsBoxContent)

-- | Create a hint media header data box.
sampleDescription
  :: (KnownNat (Length ts))
  => Boxes ts
  -> Box (ContainerBox (FullBox SampleDescription 0) ts)
sampleDescription bs =
  containerBox (FullBox 0 $ SampleDescription (typeListLength bs))
               bs

instance IsBox SampleDescription

type instance BoxTypeSymbol SampleDescription = "stsd"
