-- | A filler box with a specific size.
module Data.ByteString.IsoBaseFileFormat.Boxes.Skip where

import qualified Data.ByteString as B
import Data.ByteString.IsoBaseFileFormat.Box
import Data.ByteString.IsoBaseFileFormat.ReExports

-- | Contents of a 'skip' box are just any number of filler bytes.
newtype Skip = Skip B.ByteString

instance IsBox Skip where
  type BoxContent Skip = Skip

type instance BoxTypeSymbol Skip = "skip"

-- | Create a 'Skip' with a given size.
skipBox :: Skip -> Box Skip
skipBox = Box

instance IsBoxContent Skip where
  boxSize (Skip bs) = fromIntegral $ B.length bs
  boxBuilder (Skip bs) = byteString bs
