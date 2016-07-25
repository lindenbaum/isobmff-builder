-- | This module re-exports all modules needed to build /ISOBMFF/ documents.
module Data.ByteString.IsoBaseFileFormat.Boxes
  (module Data.ByteString.IsoBaseFileFormat.Boxes, module X)
  where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box as X
import Data.ByteString.IsoBaseFileFormat.Boxes.BoxFields as X
import Data.ByteString.IsoBaseFileFormat.Boxes.FileType as X
import Data.ByteString.IsoBaseFileFormat.Boxes.FullBox as X
import Data.ByteString.IsoBaseFileFormat.Boxes.MediaData as X
import Data.ByteString.IsoBaseFileFormat.Boxes.MovieBox as X
import Data.ByteString.IsoBaseFileFormat.Boxes.ProgressiveDownloadInformation as X
import Data.ByteString.IsoBaseFileFormat.Boxes.Skip as X
import Data.ByteString.IsoBaseFileFormat.Boxes.Time as X
import Data.ByteString.IsoBaseFileFormat.Boxes.TrackBox as X
import Data.ByteString.IsoBaseFileFormat.Boxes.Brand as X
import qualified Data.ByteString.Lazy as BL

import Data.Int as X
import Data.Kind as X  (Type, Constraint)
import Data.Maybe as X
import Data.Type.Bool as X
import Data.Type.Equality as X
import Text.Printf as X
import Data.Default as X

-- * Modular builder

-- | A complete media file 'Builder', consisting of top-level boxes.
isobmffBuilder :: Boxes ts -> Builder
isobmffBuilder = rawContentBuilder

-- | A complete media file 'ByteString', like 'isobmffBuilder'
isobmff :: Boxes ts -> BL.ByteString
isobmff = toLazyByteString . isobmffBuilder

-- | A builder for any 'IsBoxContent'.
rawContentBuilder :: IsBoxContent c => c -> Builder
rawContentBuilder = boxBuilder

-- | A complete raw file 'ByteString', like 'rawContentBuilder'
rawContent :: IsBoxContent c => c -> BL.ByteString
rawContent = toLazyByteString . rawContentBuilder
