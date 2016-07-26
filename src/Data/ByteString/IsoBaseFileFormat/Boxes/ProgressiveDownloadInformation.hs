module Data.ByteString.IsoBaseFileFormat.Boxes.ProgressiveDownloadInformation
       where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box
import Data.ByteString.IsoBaseFileFormat.Boxes.BoxFields
import Data.ByteString.IsoBaseFileFormat.Boxes.FullBox

-- | A Box with progressive download information
data ProgressiveDownload

instance IsBoxType ProgressiveDownload where
  type BoxContent ProgressiveDownload = FullBox 0 ProgressiveDownloadContent
  toBoxType _ _ = StdType "pdin"

-- | Information for progressive media data download/playback encompasses the
-- delay for initial playback and expected download bit rate.
type ProgressiveDownloadContent = U32 "rate" :+ U32 "delay"

-- | Construct a @pdin@ box.
pdinBox
  :: ValidBox brand ProgressiveDownload
  => ProgressiveDownloadContent -> Box brand ProgressiveDownload
pdinBox = closedFullBox Default 0
