-- | A filler box with a specific size.
module Data.ByteString.IsoBaseFileFormat.Boxes.Skip where

import Data.ByteString.IsoBaseFileFormat.Box
import Data.ByteString.IsoBaseFileFormat.ReExports

-- | Contents of a 'skip' box are just any number of filler bytes.
newtype Skip = Skip Int

instance IsBox Skip where
  type BoxContent Skip = Skip

type instance BoxTypeSymbol Skip = "skip"

-- | Create a 'Skip' with a given size.
skipBox :: Skip -> Box Skip
skipBox = Box

instance IsBoxContent Skip where
  boxSize (Skip bs) = fromIntegral bs
  boxBuilder (Skip bs) = mconcat (replicate bs (word8 0))
