-- | Meta data for a presentation of a /movie/.
module Data.ByteString.IsoBaseFileFormat.Boxes.TrackBox where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box
import Data.ByteString.IsoBaseFileFormat.Boxes.BoxFields
import Data.ByteString.IsoBaseFileFormat.Boxes.FullBox

-- * @trak@ Box

-- | Compose a 'Track' box from the given boxes.
track :: ValidBoxes (Track version) ts
      => Boxes ts -> Box (Track version)
track = containerBox

-- | Container box for tracks.
data Track (version :: Nat)

instance IsBoxType' (Track version) where
  toBoxType' _ = StdType "trak"

instance BoxRules (Track version) where
  type IsTopLevelBox (Track version) = 'False
  type GetCardinality (Track version) any = 'ExactlyOne

-- * @tkhd@ Box


-- | Create a 'TrackHeader' box.
trackHeader
  :: KnownNat version => TrackHeader version -> Box (TrackHeader version)
trackHeader = closedFullBox Default 0

-- | Track meta data, indexed by a version.
data TrackHeader (version :: Nat) where
        TrackHeader ::
               Versioned TrackHeaderTimesV0
                         TrackHeaderTimesV1
                         version
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

instance KnownNat version => IsBoxType' (TrackHeader version) where
  type BoxContent (TrackHeader version) = FullBox version (TrackHeader version)
  toBoxType' _ = StdType "tkhd"

instance BoxRules (TrackHeader version) where
  type IsTopLevelBox (TrackHeader version) = 'False
  type GetCardinality (TrackHeader version) any = 'ExactlyOne
