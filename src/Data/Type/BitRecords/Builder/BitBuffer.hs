{-# LANGUAGE UndecidableInstances #-}
module Data.Type.BitRecords.Builder.BitBuffer where

import Data.Type.BitRecords.Arithmetic
import Data.Type.BitRecords.Builder.Alignment
import Data.Bits
import Data.Proxy
import GHC.TypeLits
import Data.Int
import Prelude hiding ((.), id)

-- | Types which contain a finite amount of bits, which can be set from a value
-- and an offset. Bits can be written to the value.
class ( KnownAlignment (BitBufferAlignment b)
      , Num b, Bits b, FiniteBits b, Eq b, Ord b
      ) => IsBitBuffer b where
  type BitBufferAlignment b :: Alignment
  -- | Copy bits starting at a specific offset from one @a@ the the other.
  bufferBits
    :: Int  -- ^ @length@ of the value to write in number of bits.
    -> b -- ^ The value to write (in the lower @length@ bits).
    -> Int  -- ^ The start offset in the output value
    -> b -- ^ The input to write to
    -> (b, Int, Int, b) -- ^ The output buffer, space left in buffer, the
                        -- number of remaining bits that did not fit in the
                        -- buffer, and finally the left bits themselves.

  -- | Like 'bufferBits' but work on an 'Integer' as bit source. This makes
  -- writing more the larger number of bits than 'BitBufferAlignment' more
  -- efficient, than 'bufferBits', because the input and rest bits don't need to
  -- be constantly converted with 'fromIntegral'.
  bufferBitsInteger
    :: Int  -- ^ @length@ of the value to write in number of bits.
    -> Integer -- ^ The value to write (in the lower @length@ bits).
    -> Int  -- ^ The start offset in the output value
    -> b -- ^ The input to write to
    -> (b, Int, Int, Integer) -- ^ The output buffer, space left in buffer, the
                        -- number of remaining bits that did not fit in the
                        -- buffer, and finally the left bits themselves.

type BitBufferSize b = GetAlignmentBits (BitBufferAlignment b)

instance (KnownAlignment a) => IsBitBuffer (BitBuffer a) where
  type BitBufferAlignment (BitBuffer a) = a
  -- | Copy bits starting at a specific offset from one @a@ the the other.
  -- Set bits starting from the most significant bit to the least.
  --   For example @writeBits m 1 <> writeBits n 2@ would result in:
  -- @
  --         MSB                                             LSB
  --    Bit: |k  ..  k-(m+1)|k-m  ..  k-(m+n+1)| k-(m+n)  ..  0|
  --  Value: |0     ..     1|0        ..     10|  ...          |
  --          ->             ->                 ->     (direction of writing)
  -- @
  bufferBits !len !bits !offset !buff =
    let buffLen = fromIntegral $ natVal (Proxy :: Proxy (GetAlignmentBits a))
        !spaceAvailable = buffLen - offset
        !writeLen = min spaceAvailable len
        !spaceLeft = spaceAvailable - writeLen
        !writeOffset = spaceLeft
        !restLen = len - writeLen
        !restBits = bits .&. (1 `unsafeShiftL` restLen - 1)
        !buff' = buff .|. (bits `unsafeShiftR` restLen `unsafeShiftL` writeOffset)
        in (buff', spaceLeft, restLen, restBits)
  bufferBitsInteger !len !bits !offset !buff =
    let buffLen = fromIntegral $ natVal (Proxy :: Proxy (GetAlignmentBits a))
        !spaceAvailable = buffLen - offset
        !writeLen = min spaceAvailable len
        !spaceLeft = spaceAvailable - writeLen
        !writeOffset = spaceLeft
        !restLen = len - writeLen
        !restBits = bits .&. (1 `unsafeShiftL` restLen - 1)
        !buff' = buff .|. fromIntegral (bits `unsafeShiftR` restLen `unsafeShiftL` writeOffset)
        in (buff', spaceLeft, restLen, restBits)

-- | A bitbuffer that holds as much bits as an aligned word.
newtype BitBuffer (a :: Alignment) = BitBuffer {unBitBuffer :: ToAlignedWord a}

type BitBuffer8 = BitBuffer 'Align8
type BitBuffer16 = BitBuffer 'Align16
type BitBuffer32 = BitBuffer 'Align32
type BitBuffer64 = BitBuffer 'Align64

deriving instance Eq (ToAlignedWord a) => Eq (BitBuffer a)
deriving instance Ord (ToAlignedWord a) => Ord (BitBuffer a)
deriving instance Num (ToAlignedWord a) => Num (BitBuffer a)
deriving instance Bits (ToAlignedWord a) => Bits (BitBuffer a)
deriving instance FiniteBits (ToAlignedWord a) => FiniteBits (BitBuffer a)
