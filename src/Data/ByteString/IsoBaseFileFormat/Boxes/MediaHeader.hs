-- | Media-independent properties of a tracks media content.
module Data.ByteString.IsoBaseFileFormat.Boxes.MediaHeader where

import Data.ByteString.IsoBaseFileFormat.Box
import Data.ByteString.IsoBaseFileFormat.Util.BoxFields
import Data.ByteString.IsoBaseFileFormat.Util.FullBox
import Data.ByteString.IsoBaseFileFormat.Boxes.Language
import Data.ByteString.IsoBaseFileFormat.Util.Time

-- | Media header data box.
newtype MediaHeader (v :: Nat) where
  MediaHeader
   :: Timing v :+ Language :+ Constant (I16 "pre_defined") 0
   -> MediaHeader v

-- | Create a 'MediaHeader' box.
mediaHeader
  :: (KnownNat v)
  => MediaHeader v -> Box (FullBox (MediaHeader v) v)
mediaHeader = fullBox 0

instance IsBox (MediaHeader v)

type instance BoxTypeSymbol (MediaHeader v) = "mdhd"

instance IsBoxContent (MediaHeader v) where
  boxSize (MediaHeader c) = boxSize c
  boxBuilder (MediaHeader c) = boxBuilder c
