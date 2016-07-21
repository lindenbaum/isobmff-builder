-- | Meta data for a presentation of a /movie/.
module Data.ByteString.IsoBaseFileFormat.Boxes.MovieBox where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box
import Data.Kind (Type, Constraint)
import Data.Int
import Data.Type.Equality
-- import Data.Type.List
import qualified Data.Vector.Sized as Vec
import Data.Singletons
import Data.Maybe
import Data.Singletons.Prelude.List

-- | The metadata for a presentation, a single 'MovieBox' which occurs only once
-- and top-level. It is pretty empty on it's own, but it contains nested boxes
-- with all the relevant meta data.
data MovieBox = MovieBox

instance IsBoxType MovieBox where
  toBoxType _ = StdType "moov"

instance BoxRules MovieBox where
  type RequiredNestedBoxes MovieBox = '[MovieHeaderBox]

-- | A movie parent box. Combine with the nested 'Boxes' using 'boxes' or '(^-)'
--
-- Example:
-- >  xxx :: Box "moov"
-- >  xxx = movieBox
-- >         ^- Nested (movieHeaderBox (...))
-- >                   :- (trackBox
-- >                       ^- Nested (trackHeaderBox (TrackHeader ...))
-- >                       :- trackReferenceBox (TrackReference ...)
-- >                       :- trackGroupingIndication (TrackGroupingInd ...))
--
-- movieBox :: ParentBox "moov"
-- movieBox = emptyParentBox

-- * @mvhd@ Box

type MovieHeaderBox = Box "mvhd"

movieHeaderBox :: MovieHeaderBox
movieHeaderBox = emptyBox

instance BoxRules "mvhd" where
  type IsTopLevelBox "mvhd" = 'False
  type GetCardinality "mvhd" any = 'ExactlyOnce
  type RestrictedTo "mvhd" = 'Just '["moov"]

  -- * @trak@ Box

type TrackHeaderBox = Box "trak"

trackHeaderBox :: TrackHeaderBox
trackHeaderBox = emptyBox

instance BoxRules "trak" where
  type IsTopLevelBox "trak" = 'False
  type GetCardinality "trak" any = 'ExactlyOnce
  type RestrictedTo "trak" = 'Just '["moov"]

-- xxx :: Box "moov"
-- xxx = movieBox
--       ^- Nested movieHeaderBox
--       :- trackHeaderBox
