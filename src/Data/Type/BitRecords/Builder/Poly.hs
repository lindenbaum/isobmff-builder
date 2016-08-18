module Data.Type.BitRecords.Builder.Poly where

import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy                   as B
import           Data.Type.BitRecords.Builder.BitBuffer
import           Text.Printf

type HasBuilder = (ToBitBufferBuilder Builder)

class ToBitBufferBuilder b where
  toByteBuilder :: BitBuffer -> b
  toBitBufferBuilder :: BitBuffer -> b

instance ToBitBufferBuilder Builder where
  toByteBuilder = word8 . fromIntegral . unBitBuffer
  toBitBufferBuilder = word64BE . unBitBuffer

newtype LittleEndianBuilder = LittleEndianBuilder Builder
  deriving Monoid

instance ToBitBufferBuilder LittleEndianBuilder where
  toByteBuilder = LittleEndianBuilder . word8 . fromIntegral . unBitBuffer
  toBitBufferBuilder = LittleEndianBuilder . word64LE . unBitBuffer

printBuilder :: Builder -> String
printBuilder b =
      ("<< " ++)
   $  (++" >>")
   $  unwords
   $  printf "%0.2x"
  <$>  B.unpack (toLazyByteString b)
