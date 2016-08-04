-- | Media-independent properties of a tracks video content.
module Data.ByteString.IsoBaseFileFormat.Boxes.VideoMediaHeader where

import Data.ByteString.IsoBaseFileFormat.Box
import Data.ByteString.IsoBaseFileFormat.Util.BoxFields
import Data.ByteString.IsoBaseFileFormat.Util.FullBox
import Data.ByteString.IsoBaseFileFormat.Boxes.Handler
import Data.ByteString.IsoBaseFileFormat.Boxes.SpecificMediaHeader

type instance MediaHeaderFor 'VideoTrack = VideoMediaHeader

-- | Video header data box.
newtype VideoMediaHeader where
  VideoMediaHeader
   :: Template (U16 "graphicsmode") 0
   :+ Template (U16Arr "opcolor" 3) '[0,0,0]
   -> VideoMediaHeader
   deriving (Default, IsBoxContent)

-- | Create a video media header data box.
videoMediaHeader :: VideoMediaHeader -> Box (FullBox VideoMediaHeader 0)
videoMediaHeader = fullBox 1

instance IsBox VideoMediaHeader
type instance BoxTypeSymbol VideoMediaHeader = "vmhd"
