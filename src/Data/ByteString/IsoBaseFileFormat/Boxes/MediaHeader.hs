module Data.ByteString.IsoBaseFileFormat.Boxes.MediaHeader where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box
import Data.ByteString.IsoBaseFileFormat.Boxes.BoxFields
import Data.ByteString.IsoBaseFileFormat.Boxes.Time
import Data.ByteString.IsoBaseFileFormat.Boxes.Language

-- | Media header data box
data MediaHeader (v :: Nat) where
  MediaHeader
   :: Timing v :+ Language :+ Constant (I16 "pre_defined") 0
   -> MediaHeader v

-- | Create a 'MediaDataBox' from a strict 'ByteString'
mediaHeader
  :: (ValidBox brand (MediaHeader v))
  => MediaHeader v -> Box brand (MediaHeader v)
mediaHeader = closedBox

instance IsBoxType' (MediaHeader v) where
  type BoxContent (MediaHeader v) = MediaHeader v
  toBoxType' _ = StdType "mdhd"

instance IsBoxContent (MediaHeader v) where
  boxSize (MediaHeader c) = boxSize c
  boxBuilder (MediaHeader c) = boxBuilder c
