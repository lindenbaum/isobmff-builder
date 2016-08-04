-- | A box containing the sample count and sample size table.
module Data.ByteString.IsoBaseFileFormat.Boxes.SampleSize where

import           Data.ByteString.IsoBaseFileFormat.Box
import           Data.ByteString.IsoBaseFileFormat.Util.FullBox
import Data.ByteString.IsoBaseFileFormat.ReExports

-- | Content type of the 'FullBox' containing the 'SampleSize'.
type SampleSize = FullBox SampleSizeTable 0

-- | Create a 'SampleSize' box for the case where all sample sizes are equal, no
-- table with the size of each sample is required.
fixedSampleSize :: Word32 -> Word32 -> Box SampleSize
fixedSampleSize sampleSize sampleCount =
  fullBox 0 $ FixedSampleSize sampleSize sampleCount

-- | Create a 'SampleSize' box for the case where samples have different sizes.
-- The list MUST contain an entry for **every sample** in the media file.
individualSampleSizes :: [Word32] -> Box SampleSize
individualSampleSizes sampleSizes =
  fullBox 0 $
    SampleSizeTable (ListContent sampleSizes)

-- | The 'SampleSize' box content. If @sampleSize == 0@ then each sample has its
-- own size and the @table@ field contains an entry for **every** sample, the
-- number of entries is stored in @sampleCount@. Otherwise the samples are
-- assumed to all have the same size: @sampleSize@.
data SampleSizeTable =
    FixedSampleSize { sampleSize :: !Word32, sampleCount :: !Word32 }
  | SampleSizeTable !(ListContent Word32 Word32)

instance IsBox SampleSizeTable

type instance BoxTypeSymbol SampleSizeTable = "stsz"

instance IsBoxContent SampleSizeTable where
  boxSize (FixedSampleSize _ _) = 8
  boxSize (SampleSizeTable entries) = 4 + boxSize entries
  boxBuilder (FixedSampleSize size count) = word32BE size <> word32BE count
  boxBuilder (SampleSizeTable entries) = word32BE 0 <> boxBuilder entries

-- TODO implement compact sample size box (stz2)
