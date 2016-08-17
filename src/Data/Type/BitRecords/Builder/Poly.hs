module Data.Type.BitRecords.Builder.Poly where

import Data.Type.BitRecords.Builder.Alignment
import Data.Type.BitRecords.Builder.BitBuffer
import Data.Word
import Data.ByteString.Builder

type HasBuilder a = (ToBitBufferBuilder a Builder)

class ToBitBufferBuilder (a :: Alignment) b where
  toByteBuilder :: BitBuffer a -> b
  toBitBufferBuilder :: BitBuffer a -> b

instance ToBitBufferBuilder 'Align8 Builder where
  toByteBuilder = word8 . unBitBuffer
  toBitBufferBuilder = word8 . unBitBuffer
instance ToBitBufferBuilder 'Align16 Builder where
  toByteBuilder = word8 . fromIntegral . unBitBuffer
  toBitBufferBuilder = word16BE . unBitBuffer
instance ToBitBufferBuilder 'Align32 Builder where
  toByteBuilder = word8 . fromIntegral . unBitBuffer
  toBitBufferBuilder = word32BE . unBitBuffer
instance ToBitBufferBuilder 'Align64 Builder where
  toByteBuilder = word8 . fromIntegral . unBitBuffer
  toBitBufferBuilder = word64BE . unBitBuffer

newtype LittleEndianBuilder = LittleEndianBuilder Builder
  deriving Monoid

instance ToBitBufferBuilder 'Align16 LittleEndianBuilder where
  toByteBuilder = LittleEndianBuilder . word8 . fromIntegral . unBitBuffer
  toBitBufferBuilder = LittleEndianBuilder . word16BE . unBitBuffer
instance ToBitBufferBuilder 'Align32 LittleEndianBuilder where
  toByteBuilder = LittleEndianBuilder . word8 . fromIntegral . unBitBuffer
  toBitBufferBuilder = LittleEndianBuilder . word32BE . unBitBuffer
instance ToBitBufferBuilder 'Align64 LittleEndianBuilder where
  toByteBuilder = LittleEndianBuilder . word8 . fromIntegral . unBitBuffer
  toBitBufferBuilder = LittleEndianBuilder . word64BE . unBitBuffer
