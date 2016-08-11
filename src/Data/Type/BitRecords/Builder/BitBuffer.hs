module Data.Type.BitRecords.Builder.BitBuffer where

import Data.Type.BitRecords.Arithmetic
import Data.Type.BitRecords.Builder.Alignment
import Data.Bits
import Data.Proxy
import GHC.TypeLits
import Data.Int
import Prelude hiding ((.), id)

-- | A wrapper around an integral type retreived from 'ToAlignedWord' like
-- 'Word32', 'Word64', etc, that acts as a buffer for efficient serialization of
-- bits to a 'Builder'.
newtype BitBuffer (a :: Alignment) (endianness :: Endianness) =
  BitBuffer {fromBitBufferMsbFirst :: ToAlignedWord a}

-- | Types which contain a finite amount of bits, which can be set from a value
-- and an offset. Bits can be written to the value.
type IsBitBuffer a e =
  ( KnownAlignment a
  , Num (ToAlignedWord a)
  , Show (ToAlignedWord a)
  , FiniteBits (ToAlignedWord a)
  , Bits (ToAlignedWord a)
  , KnownNat (GetAlignmentBits a))

type family BitBufferSize b :: Nat where
  BitBufferSize (BitBuffer a e) = GetAlignmentBits a

-- | Return the static size of an 'IsBitBuffer'. The parameter is ignored!
bitBufferSize :: forall a e . (IsBitBuffer a e) => BitBuffer a e -> Int
bitBufferSize _ = fromIntegral $ natVal (Proxy :: Proxy (GetAlignmentBits a))

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
  :: (IsBitBuffer a e)
  => Int  -- ^ @length@ of the value to write in number of bits.
  -> BitBuffer a e -- ^ The value to write (in the lower @length@ bits).
  -> Int  -- ^ The start offset in the output value
  -> BitBuffer a e -- ^ The input to write to
  -> (BitBuffer a e, Int, Int, BitBuffer a e) -- ^ The output buffer, space left in buffer, the
                      -- number of remaining bits that did not fit in the
                      -- buffer, and finally the left bits themselves.
bufferBits !len !bits !offset !buff =
  let !buffLen = bitBufferSize buff
      !spaceAvailable = buffLen - offset
      !writeLen = min spaceAvailable len
      !spaceLeft = spaceAvailable - writeLen
      !writeOffset = spaceLeft
      !restLen = len - writeLen
      !restBits = bits .&. (1 `unsafeShiftL` restLen - 1)
      !buff' = buff .|. (bits `unsafeShiftR` restLen `unsafeShiftL` writeOffset)
      in (buff', spaceLeft, restLen, restBits)

type NextBitBufferOffset a len offset = (len + offset) `Rem` BitBufferSize a

deriving instance Eq (ToAlignedWord a) => Eq (BitBuffer a e)
deriving instance Ord (ToAlignedWord a) => Ord (BitBuffer a e)
deriving instance Num (ToAlignedWord a) => Num (BitBuffer a e)
deriving instance Bits (ToAlignedWord a) => Bits (BitBuffer a e)
deriving instance FiniteBits (ToAlignedWord a) => FiniteBits (BitBuffer a e)
