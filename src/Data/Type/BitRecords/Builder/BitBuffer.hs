{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Data.Type.BitRecords.Builder.BitBuffer
    ( type BitStringMaxLength
    , type ModuloBitStringMaxLength
    , bitStringMaxLength
    , bitStringMaxLengthBytes
    , BitString()
    , bitStringContent
    , bitStringLength
    , isBitStringEmpty
    , bitStringSpaceLeft
    , bitString
    , emptyBitString
    , bitStringProxyLength
    , BitStringBuilderChunk()
    , bitStringBuilderChunkContent
    , bitStringBuilderChunkLength
    , isBitStringBuilderChunkEmpty
    , bitStringBuilderChunkSpaceLeft
    , bitStringBuilderChunk
    , emptyBitStringBuilderChunk
    , bufferBits
    , type KnownChunkSize
    ) where

import           Data.Proxy

import           Data.Type.BitRecords.Arithmetic
import           Data.Bits
import           Data.Word
import           Data.Kind ( Constraint )
import           GHC.TypeLits

-- | The maximum number of bits a 'BitBuffer' can hold.
type BitStringMaxLength = 64

-- | Calculate the modulus of a number and the 'BitStringMaxLength'.
type family ModuloBitStringMaxLength (len :: Nat) :: Nat where
        ModuloBitStringMaxLength len = len `RemPow2` 6

-- | The maximum number of bits a 'BitBuffer' can hold.
bitStringMaxLength :: Num a => a
bitStringMaxLength = 64

-- | The maximum number of bytes a 'BitBuffer' can hold.
bitStringMaxLengthBytes :: Word64
bitStringMaxLengthBytes = 8

-- | A string of bits with a given length (but always @<= 'bitStringMaxLength'@.
-- The number of bits must be smaller that 'bitStringMaxLength'.
data BitString = BitString !Word64 !Int

bitStringContent :: BitString -> Word64
bitStringContent (BitString !c _) =
    c

bitStringLength :: BitString -> Int
bitStringLength (BitString _ !len) =
    len

isBitStringEmpty :: BitString -> Bool
isBitStringEmpty (BitString _ !len) =
    len == 0

bitStringSpaceLeft :: BitString -> Int
bitStringSpaceLeft (BitString _ !len) =
    bitStringMaxLength - len

-- | Create a 'BitString' containing @len@ bits from LSB to MSB, properly
-- masked, such that only @len@ least significant bits are kept..
bitString :: Int -> Word64 -> BitString
bitString !len !b = BitString (let !s = bitStringMaxLength - len in ((b `unsafeShiftL` s) `unsafeShiftR` s)) len

-- | Create an empty 'BitString'.
emptyBitString :: BitString
emptyBitString = BitString 0 0

-- | A buffer for 64 bits, such that the bits are written MSB to LSB.
--
-- > type TwoFields = "f0" @: Field m :>: "f1" @: Field n
--
-- Writes:
-- @       MSB                                             LSB
--    Bit: |k  ..  k-(m+1)|k-m  ..  k-(m+n+1)|k-(m+n)  ..  0|
--  Value: |------f0------|--------f1--------|XXXXXXXXXXXXXX|
-- @
--
-- Where @k@ is the current bit offset.
-- The input values are expected to be in the order of the fields, i.e.:
--
-- @
-- runHoley $ bitStringBuilderHoley (Proxy :: Proxy TwoFields) 1 2
-- @
--
-- Will result in:
-- @       MSB                                             LSB
--    Bit: |k  ..  k-(m+1)|k-m  ..  k-(m+n+1)| k-(m+n)  ..  0|
--  Value: |0     ..     1|0       ..      10| X    ..      X|
-- @
data BitStringBuilderChunk = BitStringBuilderChunk !Word64 !Int

bitStringBuilderChunkContent :: BitStringBuilderChunk -> Word64
bitStringBuilderChunkContent (BitStringBuilderChunk !c _) =
    c

bitStringBuilderChunkLength :: BitStringBuilderChunk -> Int
bitStringBuilderChunkLength (BitStringBuilderChunk _ !len) =
    len

isBitStringBuilderChunkEmpty :: BitStringBuilderChunk -> Bool
isBitStringBuilderChunkEmpty (BitStringBuilderChunk _ !len) =
    len == 0

bitStringBuilderChunkSpaceLeft :: BitStringBuilderChunk -> Int
bitStringBuilderChunkSpaceLeft (BitStringBuilderChunk _ !len) =
    bitStringMaxLength - len

-- | Create a 'BitStringBuilderChunk' containing @len@ bits from LSB to MSB, properly
-- masked, such that only @len@ least significant bits are kept..
bitStringBuilderChunk :: Word64 -> Int -> BitStringBuilderChunk
bitStringBuilderChunk !b !len = BitStringBuilderChunk b len

-- | Create an empty 'BitStringBuilderChunk'.
emptyBitStringBuilderChunk :: BitStringBuilderChunk
emptyBitStringBuilderChunk = BitStringBuilderChunk 0 0

-- | Create a 'BitStringBuilderChunk' with a length given by a 'Proxy' to a type level
-- 'Nat'.
bitStringProxyLength :: (KnownChunkSize n) => Proxy n -> Word64 -> BitString
bitStringProxyLength !plen !v = bitString fieldLen v
    where
      !fieldLen = fromIntegral (natVal plen)


-- | Copy bits starting at a specific offset from one @a@ the the other.
-- Set bits starting from the most significant bit to the least.
--   For example @writeBits m 1 <> writeBits n 2@ would result in:
--
-- @
--         MSB                                             LSB
--    Bit: |k  ..  k-(m+1)|k-m  ..  k-(m+n+1)| k-(m+n)  ..  0|
--  Value: |0     ..     1|0        ..     10|  ...          |
--          ->             ->                 ->     (direction of writing)
-- @
--
bufferBits :: BitString -- ^ The value to write (in the lower @length@ bits).
           -> BitStringBuilderChunk -- ^ The input to write to
           -> (BitString, BitStringBuilderChunk) -- ^ The remaining bits that did not fit
                                        -- in the buffer and the output buffer.
bufferBits (BitString !bits !len) (BitStringBuilderChunk !buff !offset) =
    let !spaceAvailable = bitStringMaxLength - offset
        !writeLen = min spaceAvailable len
        !writeOffset = spaceAvailable - writeLen
        !restLen = len - writeLen
        !restBits = bits .&. (1 `unsafeShiftL` restLen - 1)
        !buff' = buff .|.
            (bits `unsafeShiftR` restLen `unsafeShiftL` writeOffset)
    in
        (BitString restBits restLen, BitStringBuilderChunk buff' (offset + writeLen))

type family KnownChunkSize (s :: Nat) :: Constraint where
        KnownChunkSize size = (KnownNat size, size <= BitStringMaxLength)
