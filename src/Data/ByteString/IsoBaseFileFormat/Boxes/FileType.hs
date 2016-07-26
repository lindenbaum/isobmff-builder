module Data.ByteString.IsoBaseFileFormat.Boxes.FileType where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box

-- | File Type Box
instance IsBoxType FileType where
  type BoxContent FileType = FileType
  toBoxType _ _ = StdType "ftyp"

-- | Create a 'FileTypeBox' from a major brand, a minor version and a list of
-- compatible brands
fileTypeBox :: FileType -> Box FileType
fileTypeBox = Box

-- | Contents of a 'ftyp' box are some 'FourCc' /brands/ and a version.
data FileType =
  FileType {majorBrand :: FourCc
           ,minorVersion :: Word32
           ,compatibleBrands :: [FourCc]}

instance IsBoxContent FileType where
  boxSize (FileType maj _ver comps) = boxSize maj + 4 + sum (boxSize <$> comps)
  boxBuilder (FileType maj ver comps) =
    boxBuilder maj <> word32BE ver <> mconcat (boxBuilder <$> comps)
