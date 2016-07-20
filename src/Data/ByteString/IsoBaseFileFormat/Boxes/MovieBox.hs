-- | Meta data for a presentation of a /movie/.
module Data.ByteString.IsoBaseFileFormat.Boxes.MovieBox where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box

-- | The metadata for a presentation, a single 'MovieBox' which occurs only once
-- and top-level. It is pretty empty on it's own, but it contains nested boxes
-- with all the relevant meta data.
type MovieBox = Box "moov"

instance BoxRules "moov" where
  type RequiredNestedBoxes "moov" = '[MovieHeader, "trak"]

-- | A movie parent box. Combine with the nested 'Boxes' using 'boxes' or '(^-)'
--
-- Example:
-- >  xxx :: Box "moov"
-- >  xxx = movieBox
-- >         ^- Nested (movieHeaderBox (MovieHeader ...))
-- >                   :- (trackBox
-- >                       ^- Nested (trackHeaderBox (TrackHeader ...))
-- >                                 :- trackReferenceBox (TrackReference ...)
-- >                                 :- trackGroupingIndication (TrackGroupingInd ...))
--
movieBox :: ParentBox "moov"
movieBox = emptyParentBox

-- | Overall media independent inforation, concerning the complete movie.
data MovieHeader

instance BoxRules MovieHeader where
  type IsTopLevelBox MovieHeader = 'False
  type GetCardinality MovieHeader any = 'ExactlyOnce

data MovieHeaderFields time =
  MovieHeaderFields {creationTime :: time
                    ,modificationTime :: time
                    ,timescale :: time
                    ,duration :: time
                    }
