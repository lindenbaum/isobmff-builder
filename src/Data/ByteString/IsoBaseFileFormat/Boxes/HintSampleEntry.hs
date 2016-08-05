{-# LANGUAGE UndecidableInstances #-}
-- | Format of the hint track as well as streaming protocol settings.
module Data.ByteString.IsoBaseFileFormat.Boxes.HintSampleEntry where

import Data.ByteString.IsoBaseFileFormat.Boxes.SampleEntry
import Data.ByteString.IsoBaseFileFormat.Boxes.Handler
import Data.ByteString.IsoBaseFileFormat.Box

-- | Protocol specific data. To create 'HintSampleEntry's a protocol specific
-- 'HintFields' instance must be provided
newtype HintSampleEntry protocol where
  HintSampleEntry :: protocol -> HintSampleEntry protocol

type instance GetHandlerType (HintSampleEntry protocol) = 'HintTrack
type instance BoxTypeSymbol (HintSampleEntry protocol) = BoxTypeSymbol protocol
