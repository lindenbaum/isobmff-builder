-- | Media-independent properties of a tracks media content.
module Data.ByteString.IsoBaseFileFormat.Boxes.MediaHeader where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box
import Data.ByteString.IsoBaseFileFormat.Boxes.BoxFields
import Data.ByteString.IsoBaseFileFormat.Boxes.FullBox
import Data.ByteString.IsoBaseFileFormat.Boxes.Language
import Data.ByteString.IsoBaseFileFormat.Boxes.Time

-- | Media header data box.
data MediaHeader (v :: Nat) where
  MediaHeader
   :: KnownNat v
   => Timing v :+ Language :+ Constant (I16 "pre_defined") 0
   -> MediaHeader v

-- | Create a 'MediaHeader' box.
mediaHeader
  :: (KnownNat v)
  => MediaHeader v -> Box (FullBox v (MediaHeader v))
mediaHeader = fullBox 0

instance IsBoxType (MediaHeader v) where
  type BoxContent (MediaHeader v) = MediaHeader v
  toBoxType _ _ = StdType "mdhd"

instance IsBoxContent (MediaHeader v) where
  boxSize (MediaHeader c) = boxSize c
  boxBuilder (MediaHeader c) = boxBuilder c
