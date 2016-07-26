-- | Data information container declare the location of media information of a
-- 'Track' the actual location are stored in 'DataReference's.
module Data.ByteString.IsoBaseFileFormat.Boxes.DataInformation where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box

-- | Data information box phantom type.
data DataInformation

-- | Compose a 'DataInformation' box.
dataInformation
  :: ValidContainerBox brand DataInformation ts
  => Boxes brand ts -> Box brand DataInformation
dataInformation = containerBox

instance IsBoxType DataInformation where
  toBoxType _ _ = StdType "dinf"
