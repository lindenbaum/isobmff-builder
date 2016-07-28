-- | Indicate that a tracks media content is /null/ or /dummy/ media.
module Data.ByteString.IsoBaseFileFormat.Boxes.NullMediaHeader where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box
import Data.ByteString.IsoBaseFileFormat.Boxes.FullBox
import Data.Default

-- | Null header data box.
data NullMediaHeader = NullMediaHeader

-- | Create a null media header data box.
nullMediaHeader :: Box (FullBox NullMediaHeader 0)
nullMediaHeader = fullBox 0 NullMediaHeader

instance IsBox NullMediaHeader
type instance BoxTypeSymbol NullMediaHeader = "nmhd"

instance Default NullMediaHeader where
  def = NullMediaHeader
instance IsBoxContent NullMediaHeader where
  boxSize _ = 0
  boxBuilder _ = mempty
