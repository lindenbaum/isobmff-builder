-- | Select the specific media header from the 'HandlerType'
module Data.ByteString.IsoBaseFileFormat.Boxes.SpecificMediaHeader where

--
-- import Data.ByteString.IsoBaseFileFormat.Boxes.VideoMediaHeader
import Data.ByteString.IsoBaseFileFormat.Boxes.Handler
--

-- | An open type family to select the specific media header from the
-- 'HandlerType'
type family MediaHeaderFor (t :: HandlerType)
