-- | TODO: this box is a bit of a hack due to the deadline pressure...
module Data.ByteString.IsoBaseFileFormat.Boxes.MovieFragment where

import Data.ByteString.IsoBaseFileFormat.Box

-- | Compose a set of boxes into a 'MovieFragment'
movieFragment :: Boxes ts -> Box (ContainerBox MovieFragment ts)
movieFragment = containerBox ()

-- | Movie Fragments
data MovieFragment

instance IsBox MovieFragment where
  type BoxContent MovieFragment = ()

type instance BoxTypeSymbol MovieFragment = "moof"
