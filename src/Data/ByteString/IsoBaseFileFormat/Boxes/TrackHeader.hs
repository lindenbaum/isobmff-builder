-- | Track header box
module Data.ByteString.IsoBaseFileFormat.Boxes.TrackHeader where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box
import Data.ByteString.IsoBaseFileFormat.Boxes.BoxFields
import Data.ByteString.IsoBaseFileFormat.Boxes.FullBox
import Data.ByteString.IsoBaseFileFormat.Boxes.Versioned

-- * @tkhd@ Box
-- | Create a 'TrackHeader' box.
trackHeader
  :: KnownNat version
  => TrackHeader version -> Box (FullBox version (TrackHeader version))
trackHeader = fullBox 0

-- | Track meta data, indexed by a version.
data TrackHeader (version :: Nat) where
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

-- | Time and timing information about a track (32bit version).
type TrackHeaderTimesV0 = TrackHeaderTimes (Scalar Word32)

-- | Time and timing information about a track (64bit version).
type TrackHeaderTimesV1 = TrackHeaderTimes (Scalar Word64)

-- | Time and timing information about a track.
type TrackHeaderTimes uint =
     uint "creation_time"
  :+ uint "modification_time"
  :+ U32 "track_ID"
  :+ Constant (U32 "reserved") 0
  :+ uint "duration"

instance IsBoxContent (TrackHeader version) where
  boxSize (TrackHeader c) = boxSize c
  boxBuilder (TrackHeader c) = boxBuilder c

instance IsBoxType (TrackHeader version) where
  type BoxContent (TrackHeader version) = TrackHeader version
  toBoxType _ _ = StdType "tkhd"
