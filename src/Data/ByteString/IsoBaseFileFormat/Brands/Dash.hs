-- | Predefined Box composition matching the @dash@ brand. TODO this is an
-- incomplete,  special-purpose variant of this brand, serving my personal,
-- educational, current need.
-- This is a convenient way of building documents of that kind.
module Data.ByteString.IsoBaseFileFormat.Brands.Dash
       (Dash(..), mkDash, mvhd, tkhd, module X)
       where

import Data.ByteString.IsoBaseFileFormat.Boxes as X
import Data.Kind (Type, Constraint)
import Control.Lens


-- | A 'BoxLayout' which contains the stuff needed for the 'dash' brand.
-- TODO incomplete
instance KnownNat v => IsBrand (Dash v) where
  type GetVersion (Dash v) = v
  type BoxLayout (Dash v) =
    '[ OnceMandatory FileType '[]
     , OnceMandatory (Movie v)
         '[ OnceMandatory (MovieHeader v) '[]
          , SomeMandatory (Track v)
              '[OnceMandatory (TrackHeader v) '[]]
          ]
     , SomeOptional Skip '[]
     ]



-- | A record which contains the stuff needed for the 'dash' brand. TODO
-- incomplete
data Dash (version :: Nat) =
  Dash {_mvhd :: MovieHeader version
       ,_tkhd :: TrackHeader version
       }

-- Missing Boxes
--  mdia
--  mdhd
--  hdlr
--  soun
--  minf
--  smhd
--  dinf
--  dref
--  url
--  stbl
--  stsd
--  mp4a
--  esds
--  stts
--  stsc
--  stsz
--  stco
--  mvex
--  trex
-- For media
-- styp
-- moof
-- mfhd
-- traf
-- tfhd
-- trun


type Todo = ()

makeLenses ''Dash

-- | Convert a 'Dash' record to a generic 'Boxes' collection.
mkDash
  :: KnownNat v
  => Dash v -> MediaFile (Dash v)
mkDash doc =
  MediaFile
      $ fileTypeBox (FileType "iso5" 0 ["isom","iso5","dash","mp42"])
     .:. movie
           $ movieHeader (doc ^. mvhd)
          .:. track
                $: trackHeader (doc ^. tkhd)
