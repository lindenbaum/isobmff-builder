-- | Media information container for all boxes declaring the specific
-- technicalities of the media data associated with a 'Track'.
module Data.ByteString.IsoBaseFileFormat.Boxes.MediaInformation where

import Data.ByteString.IsoBaseFileFormat.Box

-- | Media information box type.
data MediaInformation

-- | Compose a 'MediaInformation' box.
mediaInformation
  :: Boxes ts -> Box (ContainerBox MediaInformation ts)
mediaInformation = containerBox ()

instance IsBox MediaInformation where
  type BoxContent MediaInformation = ()

type instance BoxTypeSymbol MediaInformation = "minf"
