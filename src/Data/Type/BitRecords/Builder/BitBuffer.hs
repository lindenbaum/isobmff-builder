{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Data.Type.BitRecords.Builder.BitBuffer
    ( BitBuffer(..)
    , type BitBufferSize
    , type ModuloBitBufferSize
    , bitBufferSize
    , BittrBuffer()
    , bittrBufferContent
    , bittrBufferLength
    , isBittrBufferEmpty
    , bittrBufferSpaceLeft
    , bittrBuffer
    , emptyBittrBuffer
    , bittrBufferProxyLength
    , BitOutBuffer()
    , bitOutBufferContent
    , bitOutBufferLength
    , isBitOutBufferEmpty
    , bitOutBufferSpaceLeft
    , bitOutBuffer
    , emptyBitOutBuffer
    , bufferBits
    , BittrBufferUnlimited()
    , bittrBufferUnlimitedContent
    , bittrBufferUnlimitedLength
    , isBittrBufferUnlimitedEmpty
    , bittrBufferUnlimited
    , emptyBittrBufferUnlimited
    , bufferBitsInteger
    , type StaticBittrBufferSpaceLeft
    , type AppendRestLen
    , type AppendNewBuffOffset
    , type KnownBitBufferSize
    ) where

import Data.Proxy
import           Data.Type.Bool
import           Data.Type.BitRecords.Arithmetic
import           Data.Bits
import           Data.Word
import           Data.Kind ( Constraint )
import           GHC.TypeLits

-- | A bitbuffer that holds as much bits as an aligned word.
newtype BitBuffer = BitBuffer { unBitBuffer :: Word64 }
    deriving (Eq, Ord, Num, Bits, FiniteBits, Show)

-- | The maximum number of bits a 'BitBuffer' can hold.
type BitBufferSize = 64

-- | Calculate the modulus of a number and the 'BitBufferSize'.
type family ModuloBitBufferSize (len :: Nat) :: Nat where
        ModuloBitBufferSize len = len `RemPow2` 6

-- | The maximum number of bits a 'BitBuffer' can hold.
bitBufferSize :: Num a => a
bitBufferSize = 64

-- | A 'BitBuffer' bundled with a size that indicates how many bits are defined.
-- The number of bits must be smaller that 'bitBufferSize'.
data BittrBuffer = BittrBuffer !BitBuffer !Int

bittrBufferContent :: BittrBuffer -> BitBuffer
bittrBufferContent (BittrBuffer !c _) =
    c

bittrBufferLength :: BittrBuffer -> Int
bittrBufferLength (BittrBuffer _ !len) =
    len

isBittrBufferEmpty :: BittrBuffer -> Bool
isBittrBufferEmpty (BittrBuffer _ !len) =
    len == 0

bittrBufferSpaceLeft :: BittrBuffer -> Int
bittrBufferSpaceLeft (BittrBuffer _ !len) =
    bitBufferSize - len

-- | Create a 'BittrBuffer' containing @len@ bits from LSB to MSB, properly
-- masked, such that only @len@ least significant bits are kept..
bittrBuffer :: BitBuffer -> Int -> BittrBuffer
bittrBuffer !b !len = BittrBuffer (let !s = bitBufferSize - len in ((b `unsafeShiftL` s) `unsafeShiftR` s)) len

-- | Create an empty 'BittrBuffer'.
emptyBittrBuffer :: BittrBuffer
emptyBittrBuffer = BittrBuffer 0 0

-- | An output 'BitBuffer' bundled with a size that indicates how many bits are defined.
-- The number of bits must be smaller that 'bitBufferSize'.
data BitOutBuffer = BitOutBuffer !BitBuffer !Int

bitOutBufferContent :: BitOutBuffer -> BitBuffer
bitOutBufferContent (BitOutBuffer !c _) =
    c

bitOutBufferLength :: BitOutBuffer -> Int
bitOutBufferLength (BitOutBuffer _ !len) =
    len

isBitOutBufferEmpty :: BitOutBuffer -> Bool
isBitOutBufferEmpty (BitOutBuffer _ !len) =
    len == 0

bitOutBufferSpaceLeft :: BitOutBuffer -> Int
bitOutBufferSpaceLeft (BitOutBuffer _ !len) =
    bitBufferSize - len

-- | Create a 'BitOutBuffer' containing @len@ bits from LSB to MSB, properly
-- masked, such that only @len@ least significant bits are kept..
bitOutBuffer :: BitBuffer -> Int -> BitOutBuffer
bitOutBuffer !b !len = BitOutBuffer b len

-- | Create an empty 'BitOutBuffer'.
emptyBitOutBuffer :: BitOutBuffer
emptyBitOutBuffer = BitOutBuffer 0 0

-- | Create a 'BitOutBuffer' with a length given by a 'Proxy' to a type level
-- 'Nat'.
bittrBufferProxyLength :: (KnownBitBufferSize n) => Proxy n -> Word64 -> BittrBuffer
bittrBufferProxyLength !plen !v = bittrBuffer (BitBuffer v) fieldLen
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
bufferBits :: BittrBuffer -- ^ The value to write (in the lower @length@ bits).
           -> BitOutBuffer -- ^ The input to write to
           -> (BittrBuffer, BitOutBuffer) -- ^ The remaining bits that did not fit
                                        -- in the buffer and the output buffer.
bufferBits (BittrBuffer !bits !len) (BitOutBuffer !buff !offset) =
    let !spaceAvailable = bitBufferSize - offset
        !writeLen = min spaceAvailable len
        !writeOffset = spaceAvailable - writeLen
        !restLen = len - writeLen
        !restBits = bits .&. (1 `unsafeShiftL` restLen - 1)
        !buff' = buff .|.
            (bits `unsafeShiftR` restLen `unsafeShiftL` writeOffset)
    in
        (BittrBuffer restBits restLen, BitOutBuffer buff' (offset + writeLen))

-- | A 'BitBuffer' bundled with a size, just like 'BittrBuffer' but based in an
-- 'Integer' with an nrestricted length.
data BittrBufferUnlimited =

    -- | Parameters are the content as well as the number of bits from the
    -- content.
    BittrBufferUnlimited !Integer
                         !Int

bittrBufferUnlimitedContent :: BittrBufferUnlimited -> Integer
bittrBufferUnlimitedContent (BittrBufferUnlimited !c _) =
    c

bittrBufferUnlimitedLength :: BittrBufferUnlimited -> Int
bittrBufferUnlimitedLength (BittrBufferUnlimited _ !len) =
    len

isBittrBufferUnlimitedEmpty :: BittrBufferUnlimited -> Bool
isBittrBufferUnlimitedEmpty (BittrBufferUnlimited _ !len) =
    len == 0

-- | Create a 'BittrBuffer' that is properly masked.
bittrBufferUnlimited :: Integer -> Int -> BittrBufferUnlimited
bittrBufferUnlimited !b !len =
    BittrBufferUnlimited (b .&. ((1 `unsafeShiftL` len) - 1)) len

-- | Create an empty 'BittrBufferUnlimited'.
emptyBittrBufferUnlimited :: BittrBufferUnlimited
emptyBittrBufferUnlimited =
    BittrBufferUnlimited 0 0

-- | Like 'bufferBits' but work on an 'Integer' as bit source. This makes
-- writing more the larger number of bits than 'BitBufferAlignment' more
-- efficient, than 'bufferBits', because the input and rest bits don't need to
-- be constantly converted with 'fromIntegral'.
bufferBitsInteger :: BittrBufferUnlimited -- ^ The value to write (in the lower @length@ bits).
                  -> BitOutBuffer -- ^ The buffer to write to
                  -> (BittrBufferUnlimited, BitOutBuffer) -- ^ The remaining bits that did not
                                                        -- fit in the buffer and the output
                                                        -- buffer.
bufferBitsInteger (BittrBufferUnlimited !bits !len) (BitOutBuffer !buff !offset) =
    let !spaceAvailable = bitBufferSize - offset
        !writeLen = min spaceAvailable len
        !writeOffset = spaceAvailable - writeLen
        !restLen = len - writeLen
        !restBits = bits .&. (1 `unsafeShiftL` restLen - 1)
        !buff' = buff .|.
            fromIntegral (bits `unsafeShiftR` restLen `unsafeShiftL` writeOffset)
    in
        ( BittrBufferUnlimited restBits restLen
        , BitOutBuffer buff' (offset + writeLen)
        )


type family StaticBittrBufferSpaceLeft (sb :: Nat) :: Nat where
        StaticBittrBufferSpaceLeft size = BitBufferSize - size

type family KnownBitBufferSize (s :: Nat) :: Constraint where
        KnownBitBufferSize size =
                                (KnownNat size, size <= BitBufferSize,
                                 KnownNat (StaticBittrBufferSpaceLeft size))

type family AppendRestLen (argLen :: Nat) (buffLen :: Nat) :: Nat
     where
        AppendRestLen argLen buffLen =
                                     argLen - AppendWriteLen_ argLen buffLen

type family AppendNewBuffOffset (argLen :: Nat) (buffLen :: Nat) ::
     Nat where
        AppendNewBuffOffset argLen buffLen =
                                           buffLen +
                                             If (StaticBittrBufferSpaceLeft buffLen <=? argLen)
                                               (StaticBittrBufferSpaceLeft buffLen)
                                               argLen

type family AppendWriteLen_ (argLen :: Nat) (buffLen :: Nat) :: Nat
     where
        AppendWriteLen_ argLen buffLen =
                                       If (StaticBittrBufferSpaceLeft buffLen <=? argLen)
                                         (StaticBittrBufferSpaceLeft buffLen)
                                         argLen
