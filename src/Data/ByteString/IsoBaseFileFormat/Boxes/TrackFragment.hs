-- | Meta data for fragments of a track inside a movie fragment.
module Data.ByteString.IsoBaseFileFormat.Boxes.TrackFragment where

import Data.ByteString.IsoBaseFileFormat.Box

-- * @traf@ Box

-- | Compose a 'TrackFragment' box from the given boxes.
trackFragment
  :: Boxes ts -> Box (ContainerBox TrackFragment ts)
trackFragment = containerBox ()

-- | Container box for tracks.
data TrackFragment

instance IsBox TrackFragment where
  type BoxContent TrackFragment = ()

type instance BoxTypeSymbol TrackFragment = "traf"

-- | Return the static size of the empty box
trackFragmentStaticSize :: Num a => a
trackFragmentStaticSize = fromBoxSize 0 (boxSize (trackFragment NoBoxes))
