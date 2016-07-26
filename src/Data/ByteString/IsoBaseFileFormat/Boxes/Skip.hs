-- | A filler box with a specific size.
module Data.ByteString.IsoBaseFileFormat.Boxes.Skip where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box

-- | Contents of a 'skip' box are just any number of filler bytes.
newtype Skip = Skip Int

instance IsBoxType Skip where
  type BoxContent Skip = Skip
  toBoxType _ _ = StdType "skip"

-- | Create a 'Skip' with a given size.
skipBox :: ValidBox brand Skip => Skip -> Box brand Skip
skipBox = closedBox

instance IsBoxContent Skip where
  boxSize (Skip bs) = fromIntegral bs
  boxBuilder (Skip bs) = mconcat (replicate bs (word8 0))
