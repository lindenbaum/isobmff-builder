module Data.ByteString.IsoBaseFileFormat.Boxes.ProgressiveDownloadInformation
       where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box
import Data.ByteString.IsoBaseFileFormat.Boxes.BoxFields
import Data.ByteString.IsoBaseFileFormat.Boxes.FullBox

-- | A Box with progressive download information
data ProgressiveDownload

instance IsBox ProgressiveDownload where
  type BoxContent ProgressiveDownload = ProgressiveDownloadContent

type instance BoxTypeSymbol ProgressiveDownload = "pdin"

-- | Information for progressive media data download/playback encompasses the
-- delay for initial playback and expected download bit rate.
type ProgressiveDownloadContent = U32 "rate" :+ U32 "delay"

-- | Construct a @pdin@ box.
pdinBox
  :: ProgressiveDownloadContent -> Box (FullBox 0 ProgressiveDownload)
pdinBox = fullBox 0
