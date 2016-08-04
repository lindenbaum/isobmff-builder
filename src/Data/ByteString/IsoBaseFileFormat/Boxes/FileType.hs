module Data.ByteString.IsoBaseFileFormat.Boxes.FileType where

import Data.ByteString.IsoBaseFileFormat.Box

-- | File Type Box
instance IsBox FileType where
  type BoxContent FileType = FileType
type instance BoxTypeSymbol FileType = "ftyp"

-- | Create a 'FileTypeBox' from a major brand, a minor version and a list of
-- compatible brands
fileTypeBox :: FileType -> Box FileType
fileTypeBox = Box

-- | Contents of a 'ftyp' box are some 'FourCc' /brands/ and a version.
data FileType =
  FileType {majorBrand :: !FourCc -- TODO use U32String
           ,minorVersion :: !Word32 -- TODO use U32String
           ,compatibleBrands :: ![FourCc]}  -- TODO use U32String

instance IsBoxContent FileType where
  boxSize (FileType maj _ver comps) = boxSize maj + 4 + sum (boxSize <$> comps)
  boxBuilder (FileType maj ver comps) =
    boxBuilder maj <> word32BE ver <> mconcat (boxBuilder <$> comps)
