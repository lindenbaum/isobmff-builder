-- | Meta data for a presentation of a /movie/.
module Data.ByteString.IsoBaseFileFormat.Boxes.MovieHeader where

import Data.ByteString.IsoBaseFileFormat.Box
import Data.ByteString.IsoBaseFileFormat.Util.BoxFields
import Data.ByteString.IsoBaseFileFormat.Util.FullBox
import Data.ByteString.IsoBaseFileFormat.Util.Time
import Data.ByteString.IsoBaseFileFormat.ReExports

-- * @mvhd@ Box

-- | Construct a 'MovieHeader' box.
movieHeader
  :: (KnownNat version)
  => MovieHeader version -> Box (FullBox (MovieHeader version) version)
movieHeader = fullBox 0

-- | Movie meta data, indexed by a version.
--
-- The @rate@ field is a 16.16 fix point number, 1.0 indicates the preferred
-- normal playback rate. @volume@ is also a fix point number, albeit smaller
-- with only 8.8. A volume of 1.0 is the full (loudest) volume. @matrix@ is the
-- video image transformation matrix. The @next_track_ID@  indicates the next
-- available track id, if the track ID is (0xFFFFFFFF) then the system must
-- search through all tracks associated with this presentation and figure it
-- out, 0 is not allowed.
newtype MovieHeader (version :: Nat) where
        MovieHeader ::
               Timing version
            :+ Template (I32 "rate") 0x00010000
            :+ Template (I16 "volume") 0x0100
            :+ Constant (I16 "reserved") 0
            :+ Constant (I32Arr "reserved" 2) '[0,0]
            :+ Template (I32Arr "matrix" 9)
                        '[65536, 0, 0, 0, 65536, 0, 0, 0, 1073741824]
            :+ Template (U32Arr "pre_defined" 6) '[0,0,0,0,0,0]
            :+ Template (U32 "next_track_ID") 0xFFFFFFFF
            -> MovieHeader version
    deriving (IsBoxContent)

-- | Time and timing information about a movie (32bit version).
type MovieHeaderTimesV0 = MovieHeaderTimes
                             (Scalar Word32)
                             (Template (U32 "duration") 0xffffffff)

-- | Time and timing information about a movie (64bit version).
type MovieHeaderTimesV1 = MovieHeaderTimes
                             (Scalar Word64)
                             (Template (U64 "duration") 0xffffffffffffffff)

-- | Time and timing information about a movie.
--
-- The creation/modification times are in seconds since midnight, Jan. 1, 1904,
-- in UTC time. Time scale declares the time coordinate system, it specifies the
-- number of time units that pass one second. The time coordinate system is used
-- by e.g. the duration field, which by the way contains the duration of the
-- longest track, if known, or simply the equivalent of 1s.
type MovieHeaderTimes uint dur =
      uint "creation_time"
   :+ uint "modification_time"
   :+ TimeScale
   :+ dur

instance IsBox (MovieHeader version)

type instance BoxTypeSymbol (MovieHeader v) = "mvhd"
