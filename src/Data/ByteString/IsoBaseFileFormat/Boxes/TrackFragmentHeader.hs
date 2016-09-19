-- | Meta data for a presentation of a /movie fragment/.
module Data.ByteString.IsoBaseFileFormat.Boxes.TrackFragmentHeader where

import Data.ByteString.IsoBaseFileFormat.Box
import Data.ByteString.IsoBaseFileFormat.Util.BoxFields
import Data.ByteString.IsoBaseFileFormat.Util.FullBox
import Data.Default

-- * @tfhd@ Box

-- | Construct a 'TrackFragmentHeader' box.

trackFragmentHeader -- TODO allow 64 bit variant
  :: TrackFragmentHeader -> Box (FullBox TrackFragmentHeader 0)
trackFragmentHeader =
  fullBox 0x020000 -- TODO box is hard coded for file offset calculations in the 'trun' box

-- | Track fragment meta data
-- TODO box is hard coded for file offset calculations in the 'trun' box
newtype TrackFragmentHeader where
  TrackFragmentHeader ::
    Template (U32 "track_ID") 0x001
    -> TrackFragmentHeader
    -- TODO all optional fields currently omitted for offset calculations in the 'trun' box
  deriving (IsBoxContent, Default)

instance IsBox TrackFragmentHeader

type instance BoxTypeSymbol TrackFragmentHeader = "tfhd"
