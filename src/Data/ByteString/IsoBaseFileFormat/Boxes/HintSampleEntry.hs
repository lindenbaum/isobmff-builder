{-# LANGUAGE UndecidableInstances #-}
-- | Format of the hint track as well as streaming protocol settings.
module Data.ByteString.IsoBaseFileFormat.Boxes.HintSampleEntry where

import Data.ByteString.IsoBaseFileFormat.Boxes.SampleEntry
import Data.ByteString.IsoBaseFileFormat.Boxes.Handler
import Data.ByteString.IsoBaseFileFormat.Boxes.Box

-- | Protocol specific data. To create 'HintSampleEntry's a protocol specific
-- 'HintFields' instance must be provided
data instance SampleEntry 'HintTrack protocol where
  HintSampleEntry
    :: (IsBoxContent (HintFields protocol),
        Default (HintFields protocol))
     => !(HintFields protocol)
     -> SampleEntry 'HintTrack protocol

-- | Family of protocol specific contents for 'HintTrack's.
type family HintFields (protocol :: Symbol)
