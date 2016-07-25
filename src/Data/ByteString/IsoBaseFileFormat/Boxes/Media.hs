-- | Track specific media information container box.
module Data.ByteString.IsoBaseFileFormat.Boxes.Media where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box

-- | Media data box
data Media

instance IsBoxType' Media where
  toBoxType' _ = StdType "mdia"

-- | Create a 'MediaDataBox' from a strict 'ByteString'
media :: ValidContainerBox brand Media ts
      => Boxes brand ts -> Box brand Media
media = containerBox
