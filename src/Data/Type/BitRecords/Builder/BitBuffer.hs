{-# LANGUAGE UndecidableInstances #-}
module Data.Type.BitRecords.Builder.BitBuffer where

import Data.Type.BitRecords.Arithmetic
import Data.Bits
import Data.Word
import Data.Int
import Data.Type.Bool
import GHC.TypeLits
import Prelude hiding ((.), id)


-- | A bitbuffer that holds as much bits as an aligned word.
newtype BitBuffer = BitBuffer {unBitBuffer :: Word64}
  deriving (Eq,Ord,Num,Bits,FiniteBits,Show)

type BitBufferSize = 64
bitBufferSize :: Int
bitBufferSize = 64

type family
  ModuloBitBufferSize (len :: Nat) (offset :: Nat) :: Nat where
  ModuloBitBufferSize len offset = (len + offset) `RemPow2` 6

-- | Copy bits starting at a specific offset from one @a@ the the other.
-- Set bits starting from the most significant bit to the least.
--   For example @writeBits m 1 <> writeBits n 2@ would result in:
-- @
--         MSB                                             LSB
--    Bit: |k  ..  k-(m+1)|k-m  ..  k-(m+n+1)| k-(m+n)  ..  0|
--  Value: |0     ..     1|0        ..     10|  ...          |
--          ->             ->                 ->     (direction of writing)
-- @
bufferBits
  :: Int  -- ^ @length@ of the value to write in number of bits.
  -> BitBuffer -- ^ The value to write (in the lower @length@ bits).
  -> Int  -- ^ The start offset in the output value
  -> BitBuffer -- ^ The input to write to
  -> (BitBuffer, Int, Int, BitBuffer) -- ^ The output buffer, space left in buffer, the
                                     -- number of remaining bits that did not fit in the
                                     -- buffer, and finally the left bits themselves.
bufferBits !len !bits !offset !buff =
  let !spaceAvailable = bitBufferSize - offset
      !writeLen = min spaceAvailable len
      !spaceLeft = spaceAvailable - writeLen
      !writeOffset = spaceLeft
      !restLen = len - writeLen
      !restBits = bits .&. (1 `unsafeShiftL` restLen - 1)
      !buff' = buff .|. (bits `unsafeShiftR` restLen `unsafeShiftL` writeOffset)
      in (buff', spaceLeft, restLen, restBits)

-- | Like 'bufferBits' but work on an 'Integer' as bit source. This makes
-- writing more the larger number of bits than 'BitBufferAlignment' more
-- efficient, than 'bufferBits', because the input and rest bits don't need to
-- be constantly converted with 'fromIntegral'.
bufferBitsInteger
    :: Int  -- ^ @length@ of the value to write in number of bits.
    -> Integer -- ^ The value to write (in the lower @length@ bits).
    -> Int  -- ^ The start offset in the output value
    -> BitBuffer -- ^ The input to write to
    -> (BitBuffer, Int, Int, Integer) -- ^ The output buffer, space left in buffer, the
                                     -- number of remaining bits that did not fit in the
                                     -- buffer, and finally the left bits themselves.
bufferBitsInteger !len !bits !offset !buff =
  let !spaceAvailable = bitBufferSize - offset
      !writeLen = min spaceAvailable len
      !spaceLeft = spaceAvailable - writeLen
      !writeOffset = spaceLeft
      !restLen = len - writeLen
      !restBits = bits .&. (1 `unsafeShiftL` restLen - 1)
      !buff' = buff .|. fromIntegral (bits `unsafeShiftR` restLen `unsafeShiftL` writeOffset)
  in (buff', spaceLeft, restLen, restBits)
