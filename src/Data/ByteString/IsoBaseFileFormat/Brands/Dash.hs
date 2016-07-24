-- | Predefined Box composition matching the @dash@ brand. TODO this is an
-- incomplete,  special-purpose variant of this brand, serving my personal,
-- educational, current need.
-- This is a convenient way of building documents of that kind.
module Data.ByteString.IsoBaseFileFormat.Brands.Dash
       (MinimalIsobmff(..), ftyp, mvhd, tkhd, minimalIsobmff, module X)
       where

import Data.ByteString.IsoBaseFileFormat.Boxes as X
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

-- | A record which contains the stuff needed for the 'dash' brand. TODO incomplete
data Dash (version :: Nat) =
  Dash {_ftyp :: FileType
       ,_mvhd :: MovieHeader version
       ,_tkhd :: TrackHeader version}

makeLenses ''Dash

dash
  :: KnownNat version
  => Dash version -> Boxes '[FileType, Movie version]
dash doc =
  fileTypeBox (doc ^. ftyp) .:. movie $
  movieHeader (doc ^. mvhd) .:. track $: trackHeader (doc ^. tkhd)
