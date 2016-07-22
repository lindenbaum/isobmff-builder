-- | The minimal ISO 14496-12 document
module Data.ByteString.IsoBaseFileFormat.Documents.Minimal
       (MinimalIsobmff(..), ftyp, mvhd, tkhd, minimalIsobmff) where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box
import Data.ByteString.IsoBaseFileFormat.Boxes.BoxFields
import Data.ByteString.IsoBaseFileFormat.Boxes.FileType
import Data.ByteString.IsoBaseFileFormat.Boxes.FullBox
import Data.ByteString.IsoBaseFileFormat.Boxes.MediaData
import Data.ByteString.IsoBaseFileFormat.Boxes.MovieBox
import Data.ByteString.IsoBaseFileFormat.Boxes.ProgressiveDownloadInformation
import Data.ByteString.IsoBaseFileFormat.Boxes.Skip
import qualified Data.ByteString.Lazy as BL
import Data.Int
import Data.Kind (Type, Constraint)
import Data.Maybe
import Data.Type.Bool
import Data.Type.Equality
import Text.Printf
import Control.Lens

data MinimalIsobmff (version :: Nat) =
  MinimalIsobmff {_ftyp :: FileType
                 ,_mvhd :: MovieHeader version
                 ,_tkhd :: TrackHeader version}

makeLenses ''MinimalIsobmff

minimalIsobmff
  :: KnownNat version
  => MinimalIsobmff version
  -> Boxes '[FileType, Movie version]
minimalIsobmff doc =
      fileTypeBox (doc ^. ftyp)
  .:. movie $
          movieHeader (doc ^. mvhd)
      .:. track
          .: trackHeader (doc ^. tkhd)
