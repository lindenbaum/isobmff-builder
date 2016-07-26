-- | Track specific media information container box.
module Data.ByteString.IsoBaseFileFormat.Boxes.Media where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box

-- | Media data box
data Media

instance IsBoxType Media where
  toBoxType _ _ = StdType "mdia"

-- | Create a 'MediaDataBox' from a strict 'ByteString'
media :: Boxes ts -> Box (ContainerBox Media ts)
media = containerBox
