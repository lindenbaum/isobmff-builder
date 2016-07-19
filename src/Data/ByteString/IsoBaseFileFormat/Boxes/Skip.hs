module Data.ByteString.IsoBaseFileFormat.Boxes.Skip where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box

-- | A filler box, the contents are skipped
type SkipBox = Box Skip

-- | Create a 'SkipBox' with a given size.
skipBox :: Int -> SkipBox
skipBox = box "skip" . Skip

-- | Create a 'SkipBox' with a given size. Same es 'skipBox' but with the fourcc
-- 'free' - the standard allows both 'skip' and 'free'.
freeBox :: Int -> SkipBox
freeBox = box "free" . Skip

-- | Contents of a 'skip' box are just any number of filler bytes.
newtype Skip =
  Skip Int

instance IsBoxContent Skip where
  boxSize (Skip bs) = fromIntegral bs
  boxBuilder (Skip bs) = mconcat (replicate bs (word8 0))
