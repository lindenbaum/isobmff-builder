module Data.ByteString.IsoBaseFileFormat.Boxes.ProgressiveDownloadInformation
       where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box
import Data.ByteString.IsoBaseFileFormat.Boxes.BoxFields

-- | A Box with progressive download information
data ProgressiveDownload

instance BoxRules ProgressiveDownload

instance IsBoxType' ProgressiveDownload where
  -- | Information for progressive media data download/playback encompasses the
  -- delay for initial playback and expected download bit rate.
  type BoxContent ProgressiveDownload = FullBoxHeader :+ U32 "rate" :+ U32 "delay"
  toBoxType' _ = StdType "pdin"

pdinBox
  :: BoxContent ProgressiveDownload -> Box' ProgressiveDownload
pdinBox = closedBox
