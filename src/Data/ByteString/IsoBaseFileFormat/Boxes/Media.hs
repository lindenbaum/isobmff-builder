-- | Track specific media information container box.
module Data.ByteString.IsoBaseFileFormat.Boxes.Media where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box

-- | Media data box
data Media

instance IsBox Media where
  type BoxContent Media = ()

type instance BoxTypeSymbol Media = "mdia"

-- | Create a 'MediaDataBox' from a strict 'ByteString'
media :: Boxes ts -> Box (ContainerBox Media ts)
media = containerBox ()
