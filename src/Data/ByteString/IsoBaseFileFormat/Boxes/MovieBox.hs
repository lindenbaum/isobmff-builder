-- | Meta data for a presentation of a /movie/.
module Data.ByteString.IsoBaseFileFormat.Boxes.MovieBox where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box

-- Uncomment this to see what the custom compiler errors look like:
--
-- import Data.ByteString.IsoBaseFileFormat.Boxes.Skip
-- xxx :: Box "moov"
-- xxx = movieBox
--         (movieBoxContainer :- skipBox (Skip 100))

-- | The metadata for a presentation, a single 'MovieBox' which occurs only once
-- and top-level. It is pretty empty on it's own, but it contains nested boxes
-- with all the relevant meta data.
type MovieBox = Box "moov"

instance BoxRules "moov" where
  type RequiredNestedBoxes "moov" = '["mvhd", "trak"]

-- | Compose a movie box from the required 'Boxes'.
movieBox :: forall ts. ValidBoxes "moov" ts => Boxes "moov" ts -> Box "moov"
movieBox = boxes

-- | The movie box container, use this to create movie boxes filled with nested
--   boxes, for example:
--
-- >  xxx :: Box "moov"
-- >  xxx = movieBox
-- >          (movieBoxContainer
-- >           :- movieHeaderBox (MovieHeader ...)
-- >           :- trackBox
-- >              (trackBoxContainer
-- >               :- trackHeaderBox
-- >               :- trackReferenceBox
-- >               :- trackGroupingIndication))
--
movieBoxContainer :: Container "moov"
movieBoxContainer = Parent emptyBox
