module Data.ByteString.IsoBaseFileFormat.Boxes.FileType where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box

-- | File Type Box
type FileTypeBox = Box ('StdType "ftyp")

-- | Create a 'FileTypeBox' from a major brand, a minor version and a list of
-- compatible brands
ftypBox :: FileType -> FileTypeBox
ftypBox = box 

-- | Contents of a 'ftyp' box are some 'FourCc' /brands/ and a version.
data FileType =
  FileType {majorBrand :: FourCc
           ,minorVersion :: Word32
           ,compatibleBrands :: [FourCc]}

instance IsBoxContent FileType where
  boxSize (FileType maj ver comps) = boxSize maj + 4 + sum (boxSize <$> comps)
  boxBuilder (FileType maj ver comps) =
    boxBuilder maj <> word32BE ver <> mconcat (boxBuilder <$> comps)
