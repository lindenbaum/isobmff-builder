module Data.Type.BitRecords.Builder.Poly where

import Data.Type.BitRecords.Builder.Alignment
import Data.Type.BitRecords.Builder.BitBuffer

import Data.ByteString.Builder

type HasBuilder a = (ToBitBufferBuilder a Builder)

class ToBitBufferBuilder (a :: Alignment) b where
  toBitBufferBuilder :: BitBuffer a -> b

instance ToBitBufferBuilder 'Align8 Builder where
  toBitBufferBuilder = word8 . unBitBuffer
instance ToBitBufferBuilder 'Align16 Builder where
  toBitBufferBuilder = word16BE . unBitBuffer
instance ToBitBufferBuilder 'Align32 Builder where
  toBitBufferBuilder = word32BE . unBitBuffer
instance ToBitBufferBuilder 'Align64 Builder where
  toBitBufferBuilder = word64BE . unBitBuffer

newtype LittleEndianBuilder = LittleEndianBuilder Builder
  deriving Monoid

instance ToBitBufferBuilder 'Align16 LittleEndianBuilder where
  toBitBufferBuilder = LittleEndianBuilder . word16BE . unBitBuffer
instance ToBitBufferBuilder 'Align32 LittleEndianBuilder where
  toBitBufferBuilder = LittleEndianBuilder . word32BE . unBitBuffer
instance ToBitBufferBuilder 'Align64 LittleEndianBuilder where
  toBitBufferBuilder = LittleEndianBuilder . word64BE . unBitBuffer
