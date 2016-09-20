-- | Track header box
module Data.ByteString.IsoBaseFileFormat.Boxes.TrackHeader
  (trackHeader, TrackHeader(..), TrackHeaderFlags(..)
  , type TrackHeaderTimes , type TrackHeaderTimesV0 , type TrackHeaderTimesV1 ) where

import Data.ByteString.IsoBaseFileFormat.Box
import Data.ByteString.IsoBaseFileFormat.Util.BoxFields
import Data.ByteString.IsoBaseFileFormat.Util.FullBox
import Data.ByteString.IsoBaseFileFormat.Util.Versioned
import Data.ByteString.IsoBaseFileFormat.Util.Time
import Data.ByteString.IsoBaseFileFormat.ReExports

-- * @tkhd@ Box
-- | Create a 'TrackHeader' box.
trackHeader
  :: forall version . ( KnownNat version )
  => TrackHeaderFlags
  -> TrackHeader version
  -> Box (FullBox (TrackHeader version) version)
trackHeader flags = fullBox flags'
  where
    flags' = case flags of
      TrackDisabled -> 0
      TrackEnabled -> 1
      TrackInMovie -> 3
      TrackInMovieAndPreview -> 7

data TrackHeaderFlags =
    TrackDisabled
  | TrackEnabled
  | TrackInMovie
  | TrackInMovieAndPreview


-- | Track meta data, indexed by a version.
newtype TrackHeader (version :: Nat) where
        TrackHeader ::
          Versioned TrackHeaderTimesV0 TrackHeaderTimesV1 version
            :+ Constant (I32Arr "reserved" 2) '[0, 0]
            :+ Template (I16 "layer") 0
            :+ Template (I16 "alternate_group") 0
            :+ Template (I16 "volume") 256
            :+ Constant (I16 "reserved") 0
            :+ Template (I32Arr "matrix" 9)
                        '[65536, 0, 0, 0, 65536, 0, 0, 0, 1073741824]
            :+ I32 "width"
            :+ I32 "height"
            -> TrackHeader version
  deriving (IsBoxContent)

-- | Time and timing information about a track (32bit version).
type TrackHeaderTimesV0 = TrackHeaderTimes
                          (Scalar Word32)
                          (TS32 "duration")

-- | Time and timing information about a track (64bit version).
type TrackHeaderTimesV1 = TrackHeaderTimes
                          (Scalar Word64)
                          (TS64 "duration")

-- | Time and timing information about a track.
type TrackHeaderTimes uint dur =
     uint "creation_time"
  :+ uint "modification_time"
  :+ U32 "track_ID"
  :+ Constant (U32 "reserved") 0
  :+ dur

instance IsBox (TrackHeader v)

type instance BoxTypeSymbol (TrackHeader v) = "tkhd"
