-- | Meta data for a presentation of a /movie/.
module Data.ByteString.IsoBaseFileFormat.Boxes.MovieBox where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box
-- Uncomment this to see what the custom compiler errors look like:
--
import Data.ByteString.IsoBaseFileFormat.Boxes.Skip

-- xxx :: Box "moov"
-- xxx = movieBox
--         (movieBoxContainer :- skipBox (Skip 100))
-- | The metadata for a presentation, a single 'MovieBox' which occurs only once
-- and top-level. It is pretty empty on it's own, but it contains nested boxes
-- with all the relevant meta data.
type MovieBox = Box "moov"

instance BoxRules "moov" where
  type RequiredNestedBoxes "moov" = '["mvhd", "trak"]

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
