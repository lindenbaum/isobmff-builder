-- | Media information container for all boxes declaring the specific
-- technicalities of the media data associated with a 'Track'.
module Data.ByteString.IsoBaseFileFormat.Boxes.MediaInformation where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box

-- | Media information box phantom type.
data MediaInformation

-- | Compose a 'MediaInformation' box.
mediaInformation
  :: ValidContainerBox brand MediaInformation ts
  => Boxes brand ts -> Box brand MediaInformation
mediaInformation = containerBox

instance IsBoxType MediaInformation where
  toBoxType _ _ = StdType "minf"
