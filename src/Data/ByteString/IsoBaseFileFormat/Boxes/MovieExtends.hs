-- | Indicate the possibility of 'MovieFragment' boxes in the current file; the
-- total samples are comprised of those fragments, so they must be parsed by the
-- player.
module Data.ByteString.IsoBaseFileFormat.Boxes.MovieExtends where

import Data.ByteString.IsoBaseFileFormat.Box

-- * @mvex@ Box

-- | Indicate that the media fragmented; more detailed info is obtained by the
-- extra boxes contained in this box, e.g. the 'MovieExtendsHeader' or the
-- 'TrackExtends' boxes.
movieExtends :: Boxes ts -> Box (ContainerBox MovieExtends ts)
movieExtends = containerBox ()

-- | Phantom type to indicate @mvex@ boxes, see ISO-14496-12 8.8.1
data MovieExtends

instance IsBox MovieExtends where
  type BoxContent MovieExtends = ()

type instance BoxTypeSymbol MovieExtends = "mvex"
