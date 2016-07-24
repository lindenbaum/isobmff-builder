-- | Predefined Box composition matching the @dash@ brand. TODO this is an
-- incomplete,  special-purpose variant of this brand, serving my personal,
-- educational, current need.
-- This is a convenient way of building documents of that kind.
module Data.ByteString.IsoBaseFileFormat.Brands.Dash
       (Dash(..), mkDash, mvhd, tkhd, module X)
       where

import Data.ByteString.IsoBaseFileFormat.Boxes as X
-- import Data.Int
import Data.Kind (Type, Constraint)
-- import Data.Maybe
-- import Data.Type.Bool
-- import Data.Type.Equality
-- import Text.Printf
import Control.Lens

-- | A record which contains the stuff needed for the 'dash' brand. TODO incomplete
data Dash (version :: Nat) =
  Dash {_mvhd :: MovieHeader version
      ,_tkhd :: TrackHeader version}

makeLenses ''Dash

-- | Convert a 'Dash' record to a generic 'Boxes' collection.
mkDash
  :: KnownNat version
  => Dash version -> Boxes '[FileType, Movie version]
mkDash doc =
 fileTypeBox (FileType "iso5" 0 ["isom","iso5","dash","mp42"])
 .:. movie $
  movieHeader (doc ^. mvhd) .:. track $: trackHeader (doc ^. tkhd)
