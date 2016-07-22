{-# LANGUAGE UndecidableInstances #-}
-- | Meta data for a presentation of a /movie/.
module Data.ByteString.IsoBaseFileFormat.Boxes.MovieBox where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box
import Data.ByteString.IsoBaseFileFormat.Boxes.BoxFields

-- | The metadata for a presentation, a single 'Movie' which occurs only once
-- and top-level. It is pretty empty on it's own, but it contains nested boxes
-- with all the relevant meta data.
data Movie

instance IsBoxType' Movie where
  toBoxType' _ = StdType "moov"

instance BoxRules Movie where
  type RequiredNestedBoxes Movie = '[MovieHeader]

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
movieBox :: ValidBoxes Movie ts => Boxes ts -> Box' Movie
movieBox = containerBox


-- * @mvhd@ Box

data MovieHeader

instance IsBoxType' MovieHeader where
  toBoxType' _ = StdType "mvhd"
  type BoxContent MovieHeader =
       Constant (I32Arr "reserved" 2) '[0, 0]
    :+ Template (I16 "layer") 0
    :+ Template (I16 "alternate_group") 0
    :+ Template (I16 "volume") 256
    :+ Constant (I16 "reserved") 0
    :+ Template (I32Arr "matrix" 9) '[65536, 0, 0, 0, 65536, 0, 0, 0, 1073741824]
    :+ I32 "width"
    :+ I32 "height"


movieHeaderBox :: BoxContent MovieHeader -> Box' MovieHeader
movieHeaderBox = closedBox

instance BoxRules MovieHeader where
  type IsTopLevelBox MovieHeader = 'False
  type GetCardinality MovieHeader any = 'ExactlyOnce
  type RestrictedTo MovieHeader = 'Just '[Movie]

  -- * @trak@ Box

data Track

instance IsBoxType' Track where
  toBoxType' _ = StdType "trak"

instance BoxRules Track where
  type IsTopLevelBox Track = 'False
  type GetCardinality Track any = 'ExactlyOnce
  type RestrictedTo Track = 'Just '[Movie]

track :: ValidBoxes Track ts => Boxes ts -> Box' Track
track = containerBox

  -- * @tkhd@ Box

data TrackHeader

instance IsBoxType' TrackHeader where
  type BoxContent TrackHeader = U32 "FOO"
  toBoxType' _ = StdType "tkhd"

instance BoxRules TrackHeader where
  type IsTopLevelBox TrackHeader = 'False
  type GetCardinality TrackHeader any = 'ExactlyOnce
  type RestrictedTo TrackHeader = 'Just '[Track]

trackHeader :: BoxContent TrackHeader -> Box' TrackHeader
trackHeader = closedBox

-- xxx :: Box "moov"
-- xxx = movieBox
--       ^- Nested movieHeaderBox
--       :- trackHeaderBox
