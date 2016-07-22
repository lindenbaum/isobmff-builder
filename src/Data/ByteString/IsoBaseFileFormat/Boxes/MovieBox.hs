{-# LANGUAGE UndecidableInstances #-}

-- | Meta data for a presentation of a /movie/.
module Data.ByteString.IsoBaseFileFormat.Boxes.MovieBox where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box
import Data.ByteString.IsoBaseFileFormat.Boxes.BoxFields
import Data.ByteString.IsoBaseFileFormat.Boxes.FullBox

-- | The metadata for a presentation, a single 'Movie' which occurs only once
-- and top-level. It is pretty empty on it's own, but it contains nested boxes
-- with all the relevant meta data.
data Movie (version :: Nat)

instance IsBoxType' (Movie version) where
  toBoxType' _ = StdType "moov"

instance BoxRules (Movie version) where
  type RequiredNestedBoxes (Movie version) = '[MovieHeader version]

-- | A movie parent box. Combine with the nested 'Boxes using 'boxes' or '(^-)'
--
-- Example:
--
-- >  xxx :: Box' Movie
-- >  xxx = movieBox $
-- >         Nested (movieHeaderBox (...))
-- >         :. (trackBox $
-- >              Nested (trackHeaderBox (TrackHeader ...))
-- >              :. trackReferenceBox (TrackReference ...)
-- >              :. trackGroupingIndication (TrackGroupingInd ...))
--
movieBox :: ValidBoxes (Movie version) ts
         => Boxes ts -> Box' (Movie version)
movieBox = containerBox

-- * @mvhd@ Box
-- | Movie meta data, indexed by a version.
data MovieHeader (version :: Nat) where
        MovieHeader ::
            KnownNat version =>
               Constant (I32Arr "reserved" 2) '[0, 0]
            :+ Template (I16 "layer") 0
            :+ Template (I16 "alternate_group") 0
            :+ Template (I16 "volume") 256
            :+ Constant (I16 "reserved") 0
            :+ Template (I32Arr "matrix" 9)
                        '[65536, 0, 0, 0, 65536, 0, 0, 0, 1073741824]
            :+ I32 "width"
            :+ I32 "height"
            -> MovieHeader version

instance IsBoxContent (MovieHeader version) where
  boxSize (MovieHeader c) = boxSize c
  boxBuilder (MovieHeader c) = boxBuilder c

instance KnownNat version => IsBoxType' (MovieHeader version) where
  toBoxType' _ = StdType "mvhd"
  type BoxContent (MovieHeader version) = FullBox version (MovieHeader version)

-- | Construct a movie header box, with the version indicated by the type.
movieHeaderBox
  :: KnownNat version
  => MovieHeader version -> Box' (MovieHeader version)
movieHeaderBox = closedFullBox Default 0

instance BoxRules (MovieHeader version) where
  type IsTopLevelBox (MovieHeader version) = 'False
  type GetCardinality (MovieHeader version) any = 'ExactlyOnce
  type RestrictedTo (MovieHeader version) = 'Just '[Movie version]

-- * @trak@ Box
data Track (version :: Nat)

instance IsBoxType' (Track version) where
  toBoxType' _ = StdType "trak"

instance BoxRules (Track version) where
  type IsTopLevelBox (Track version) = 'False
  type GetCardinality (Track version) any = 'ExactlyOnce
  type RestrictedTo (Track version) = 'Just '[Movie version]

track :: ValidBoxes (Track version) ts
      => Boxes ts -> Box' (Track version)
track = containerBox

-- * @tkhd@ Box

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
  type GetCardinality (TrackHeader version) any = 'ExactlyOnce
  type RestrictedTo (TrackHeader version) = 'Just '[Track version]

trackHeader
  :: KnownNat version => TrackHeader version -> Box' (TrackHeader version)
trackHeader = closedFullBox Default 0
