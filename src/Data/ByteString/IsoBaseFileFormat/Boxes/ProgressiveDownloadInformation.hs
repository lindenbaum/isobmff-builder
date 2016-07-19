module Data.ByteString.IsoBaseFileFormat.Boxes.ProgressiveDownloadInformation
       where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box

-- | A Box with progressive download information
type ProgressiveDownloadInformationBox = Box (FullBox ProgressiveDownloadInformation)

-- | Create a 'pdin' box
pdinBox :: ProgressiveDownloadInformation -> ProgressiveDownloadInformationBox
pdinBox = fullBox "pdin" (BoxVersion 0) zeroBits

-- | Information for progressive media data download/playback encompasses the
-- delay for initial playback and expected download bit rate.
data ProgressiveDownloadInformation =
  ProgressiveDownloadInformation {pdinRate :: Word32 -- ^ Effective download bitrate in byte/sec
                                 ,pdinDelay :: Word32 -- ^ Delay in milli seconds
                                 }

instance IsBoxContent ProgressiveDownloadInformation where
  boxSize _ = 8
  boxBuilder pdin = word32BE (pdinRate pdin) <> word32BE (pdinDelay pdin)
