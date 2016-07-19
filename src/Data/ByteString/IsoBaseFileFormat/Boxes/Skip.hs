module Data.ByteString.IsoBaseFileFormat.Boxes.Skip where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box

-- | A filler box, the contents are skipped
type SkipBox = Box "skip"

instance BoxRules "skip" where
  type RestrictedTo "skip" = 'Nothing

-- | Create a 'SkipBox' with a given size.
skipBox :: Skip -> SkipBox
skipBox = box

-- | Contents of a 'skip' box are just any number of filler bytes.
newtype Skip =
  Skip Int

instance IsBoxContent Skip where
  boxSize (Skip bs) = fromIntegral bs
  boxBuilder (Skip bs) = mconcat (replicate bs (word8 0))
