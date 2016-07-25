-- | This module re-exports all modules needed to build /ISOBMFF/ documents.
module Data.ByteString.IsoBaseFileFormat.Boxes
  (module Data.ByteString.IsoBaseFileFormat.Boxes, module X)
  where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box as X
import Data.ByteString.IsoBaseFileFormat.Boxes.BoxFields as X
import Data.ByteString.IsoBaseFileFormat.Boxes.FileType as X
import Data.ByteString.IsoBaseFileFormat.Boxes.FullBox as X
import Data.ByteString.IsoBaseFileFormat.Boxes.Media as X
import Data.ByteString.IsoBaseFileFormat.Boxes.MediaData as X
import Data.ByteString.IsoBaseFileFormat.Boxes.MediaHeader as X
import Data.ByteString.IsoBaseFileFormat.Boxes.Movie as X
import Data.ByteString.IsoBaseFileFormat.Boxes.MovieHeader as X
import Data.ByteString.IsoBaseFileFormat.Boxes.ProgressiveDownloadInformation as X
import Data.ByteString.IsoBaseFileFormat.Boxes.Language as X
import Data.ByteString.IsoBaseFileFormat.Boxes.Skip as X
import Data.ByteString.IsoBaseFileFormat.Boxes.Time as X
import Data.ByteString.IsoBaseFileFormat.Boxes.Track as X
import Data.ByteString.IsoBaseFileFormat.Boxes.TrackHeader as X
import Data.ByteString.IsoBaseFileFormat.Boxes.Versioned as X
import qualified Data.ByteString.Lazy as BL

import Data.Int as X
import Data.Kind as X  (Type, Constraint)
import Data.Maybe as X
import Data.Type.Bool as X
import Data.Type.Equality as X
import Text.Printf as X
import Data.Default as X

-- * MediaFiles

-- | The toplevel container for all boxes of a media file.
data MediaFile brand where
  MediaFile :: (ValidTopLevel brand ts) => Boxes brand ts -> MediaFile brand

-- | Generate a lazy 'ByteString' with the contents of a 'MediaFile'
packMediaFile :: MediaFile brand -> BL.ByteString
packMediaFile (MediaFile bs) = toLazyByteString (boxBuilder bs)
