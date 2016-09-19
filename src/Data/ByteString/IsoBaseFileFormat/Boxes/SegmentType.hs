module Data.ByteString.IsoBaseFileFormat.Boxes.SegmentType where

import Data.ByteString.IsoBaseFileFormat.Box
import Data.ByteString.IsoBaseFileFormat.ReExports

-- | Segment Type Box
instance IsBox SegmentType where
  type BoxContent SegmentType = SegmentType
type instance BoxTypeSymbol SegmentType = "styp"

-- | Create a 'SegmentTypeBox' from a major brand, a minor version and a list of
-- compatible brands
segmentTypeBox :: SegmentType -> Box SegmentType
segmentTypeBox = Box

-- | Contents of a 'styp' box are some 'FourCc' /brands/ and a version.
data SegmentType =
  SegmentType {majorBrand :: !FourCc -- TODO use U32String
           ,minorVersion :: !Word32 -- TODO use U32String
           ,compatibleBrands :: ![FourCc]}  -- TODO use U32String

instance IsBoxContent SegmentType where
  boxSize (SegmentType maj _ver comps) = boxSize maj + 4 + sum (boxSize <$> comps)
  boxBuilder (SegmentType maj ver comps) =
    boxBuilder maj <> word32BE ver <> mconcat (boxBuilder <$> comps)
