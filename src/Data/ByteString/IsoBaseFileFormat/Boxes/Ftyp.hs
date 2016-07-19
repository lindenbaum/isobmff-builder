module Data.ByteString.IsoBaseFileFormat.Boxes.Ftyp where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box

-- | File Type Box
type FtypBox = Box Ftyp

-- | Create a 'FtypBox' from a major brand, a minor version and a list of
-- compatible brands
ftypBox :: Ftyp -> FtypBox
ftypBox = box "ftyp"

-- | Contents of a 'ftyp' box are some 'FourCc' /brands/ and a version.
data Ftyp =
  Ftyp {majorBrand :: FourCc
       ,minorVersion :: Word32
       ,compatibleBrands :: [FourCc]}

instance IsBoxContent Ftyp where
  boxSize (Ftyp maj ver comps) = boxSize maj + 4 + sum (boxSize <$> comps)
  boxBuilder (Ftyp maj ver comps) =
    boxBuilder maj <> word32BE ver <> mconcat (boxBuilder <$> comps)
