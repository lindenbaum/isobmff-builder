-- | Default values for duration, size and other metadata of track fragments.
-- This might allow a more efficient transfer and decoding of media.
module Data.ByteString.IsoBaseFileFormat.Boxes.TrackExtends where

import Data.ByteString.IsoBaseFileFormat.Box

import Data.ByteString.IsoBaseFileFormat.Util.FullBox
import Data.ByteString.IsoBaseFileFormat.ReExports

-- * @trex@ Box

-- | Construct a 'TrackExtends' box, with all durations and sizes set to @0@.
-- Only the track index and the index into the sample description table is
-- passed set.
trackExtendsUnknownDuration :: Word32 -> Word32 -> Box (FullBox TrackExtends 0)
trackExtendsUnknownDuration trackId descrIndex = trackExtends trackId descrIndex
  0 0 0 0 0 0 0 False 0

-- | Construct a 'TrackExtends' box from all its parameters
trackExtends
  :: Word32 -- ^ Track ID
  -> Word32 -- ^ Sample Description Index
  -> Word32 -- ^ Sample Duration
  -> Word32 -- ^ Sample Size
  -> B 2    -- ^ is leading
  -> B 2    -- ^ sample depends on
  -> B 2    -- ^ sample is depended on
  -> B 2    -- ^ sample has redundancy
  -> B 3    -- ^ padding value
  -> Bool   -- ^ is non sync sample
  -> Word16 -- ^ sample degradation priority
  -> Box (FullBox TrackExtends 0)
trackExtends =
   wrapBitBuilderBox (fullBox 0 . MkTrackExtends) (Proxy @TrackExtendsBody)

-- | Defaults for movie fragments - the type alias
newtype TrackExtends where
  MkTrackExtends :: BuilderBox -> TrackExtends
  deriving (IsBoxContent)

instance IsBox TrackExtends

type instance BoxTypeSymbol TrackExtends = "trex"

-- | Defaults for movie fragment - the content
type TrackExtendsBody =
  Eval (
     "track_ID"                         @: FieldU32
 .>: "default_sample_description_index" @: FieldU32
 .>: "default_sample_duration"          @: FieldU32
 .>: "default_sample_size"              @: FieldU32
 .>: TrackExtendsDefaultSampleFlags)

type TrackExtendsDefaultSampleFlags =
      "reserved"                    @: Field 4 := 0
  .>: "is_leading"                  @: Field 2
  .>: "sample_depends_on"           @: Field 2
  .>: "sample_is_depended_on"       @: Field 2
  .>: "sample_has_redundancy"       @: Field 2
  .>: "sample_padding_value"        @: Field 3
  .>: "sample_is_non_sync_sample"   @: Flag
  .>: "sample_degradation_priority" @: FieldU16
  .>: Return 'EmptyBitRecord
