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
    , bufferBits
    , BittrBufferUnlimited()
    , bittrBufferUnlimitedContent
    , bittrBufferUnlimitedLength
    , isBittrBufferUnlimitedEmpty
    , bittrBufferUnlimited
    , emptyBittrBufferUnlimited
    , bufferBitsInteger
    , StaticBittrBuffer()
    , staticBittrBufferContent
    , staticBittrBufferLength
    , isStaticBittrBufferEmpty
    , staticBittrBufferSpaceLeft
    , type StaticBittrBufferSpaceLeft
    , staticBittrBuffer
    , staticBittrBufferP
    , emptyStaticBittrBuffer
    , appendStaticBittrBuffer
    , type KnownBitBufferSize
    ) where

import           Data.Proxy
import           Data.Type.Bool
import           Data.Type.BitRecords.Arithmetic
import           Data.Bits
import           Data.Word
import           Data.Kind ( Constraint )
import           Data.Int
import           GHC.TypeLits
import           Prelude hiding ( (.), id )


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

-- | Create a 'BittrBuffer' that is properly masked.
bittrBuffer :: BitBuffer -> Int -> BittrBuffer
bittrBuffer !b !len = BittrBuffer (b .&. ((1 `unsafeShiftL` len) - 1)) len

-- | Create an empty 'BittrBuffer'.
emptyBittrBuffer :: BittrBuffer
emptyBittrBuffer = BittrBuffer 0 0

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
           -> BittrBuffer -- ^ The input to write to
           -> (BittrBuffer, BittrBuffer) -- ^ The remaining bits that did not fit
                                        -- in the buffer and the output buffer.
bufferBits (BittrBuffer !bits !len) (BittrBuffer !buff !offset) =
    let !spaceAvailable = bitBufferSize - offset
        !writeLen = min spaceAvailable len
        !writeOffset = spaceAvailable - writeLen
        !restLen = len - writeLen
        !restBits = bits .&. (1 `unsafeShiftL` restLen - 1)
        !buff' = buff .|.
            (bits `unsafeShiftR` restLen `unsafeShiftL` writeOffset)
    in
        (BittrBuffer restBits restLen, BittrBuffer buff' (offset + writeLen))

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
                  -> BittrBuffer -- ^ The buffer to write to
                  -> (BittrBufferUnlimited, BittrBuffer) -- ^ The remaining bits that did not
                                                        -- fit in the buffer and the output
                                                        -- buffer.
bufferBitsInteger (BittrBufferUnlimited !bits !len) (BittrBuffer !buff !offset) =
    let !spaceAvailable = bitBufferSize - offset
        !writeLen = min spaceAvailable len
        !writeOffset = spaceAvailable - writeLen
        !restLen = len - writeLen
        !restBits = bits .&. (1 `unsafeShiftL` restLen - 1)
        !buff' = buff .|.
            fromIntegral (bits `unsafeShiftR` restLen `unsafeShiftL` writeOffset)
    in
        ( BittrBufferUnlimited restBits restLen
        , BittrBuffer buff' (offset + writeLen)
        )

-- | A 'BitBuffer' with a size known at compile time. This buffer holds only
--  'BitBufferSize' bits.
newtype StaticBittrBuffer (size :: Nat) = StaticBittrBuffer BitBuffer
    deriving (Ord, Eq, Num, Bits, FiniteBits)

staticBittrBufferContent :: KnownBitBufferSize size => StaticBittrBuffer size -> BitBuffer
staticBittrBufferContent (StaticBittrBuffer !c) =
    c

staticBittrBufferLength :: KnownBitBufferSize size => StaticBittrBuffer size -> Int
staticBittrBufferLength sb =
    fromIntegral (natVal sb)

isStaticBittrBufferEmpty :: KnownBitBufferSize size => StaticBittrBuffer size -> Bool
isStaticBittrBufferEmpty sb =
    staticBittrBufferLength sb == 0

staticBittrBufferSpaceLeft :: KnownBitBufferSize size => StaticBittrBuffer size -> Int
staticBittrBufferSpaceLeft sb =
    bitBufferSize - staticBittrBufferLength sb

type family StaticBittrBufferSpaceLeft (sb :: Nat) :: Nat where
        StaticBittrBufferSpaceLeft size = BitBufferSize - size


-- | Create a 'StaticBittrBuffer' that is properly masked.
staticBittrBuffer :: (KnownNat size, size <= 64)
                  => BitBuffer
                  -> proxy size
                  -> StaticBittrBuffer size
staticBittrBuffer !b pxlen =
    StaticBittrBuffer (b .&. ((1 `unsafeShiftL` len) - 1))
  where
    !len = fromIntegral (natVal pxlen)

-- | Create a 'StaticBittrBuffer' that is properly masked.
staticBittrBufferP :: (KnownNat bits, KnownNat size, size <= 64)
                   => proxy0 bits
                   -> proxy1 size
                   -> StaticBittrBuffer size
staticBittrBufferP !pxbits !pxlen =
    StaticBittrBuffer (b .&. ((1 `unsafeShiftL` len) - 1))
  where
    !len = fromIntegral (natVal pxlen)
    !b = fromIntegral (natVal pxbits)

-- | Create an empty 'BittrBuffer'.
emptyStaticBittrBuffer :: StaticBittrBuffer 0
emptyStaticBittrBuffer =
    StaticBittrBuffer 0

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
appendStaticBittrBuffer :: forall sizeArg sizeBuff.
                        ( KnownBitBufferSize sizeBuff
                        , KnownBitBufferSize (AppendWriteLen_ sizeArg sizeBuff)
                        , KnownBitBufferSize (AppendRestLen_ sizeArg sizeBuff)
                        , KnownBitBufferSize (AppendNewBuffLen_ sizeArg sizeBuff))
                        => StaticBittrBuffer sizeArg -- ^ The value to write (in the lower @length@ bits).
                        -> StaticBittrBuffer sizeBuff -- ^ The input to write to
                        -> ( StaticBittrBuffer (AppendRestLen_ sizeArg sizeBuff)
                           , StaticBittrBuffer (AppendNewBuffLen_ sizeArg sizeBuff)
                           )-- ^ The remaining bits that did not fit in the buffer and the output
                            -- buffer.
appendStaticBittrBuffer (StaticBittrBuffer !argC) (StaticBittrBuffer !buffC) =
    let !spaceAvailable = fromIntegral (natVal (Proxy :: Proxy (StaticBittrBufferSpaceLeft sizeBuff)))
        !writeLen = fromIntegral (natVal (Proxy :: Proxy (AppendWriteLen_ sizeArg sizeBuff)))
        !writeOffset = spaceAvailable - writeLen
        !restLen = staticBittrBufferLength rest
        !buffC' = buffC .|.
            (argC `unsafeShiftR` restLen `unsafeShiftL` writeOffset)
        rest = StaticBittrBuffer (argC .&. (1 `unsafeShiftL` restLen - 1))
        buff' = StaticBittrBuffer buffC'
    in
        (rest, buff')

type family KnownBitBufferSize (s :: Nat) :: Constraint where
        KnownBitBufferSize size =
                                (KnownNat size, size <= BitBufferSize,
                                 KnownNat (StaticBittrBufferSpaceLeft size))

type family AppendRestLen_ (argLen :: Nat) (buffLen :: Nat) :: Nat
     where
        AppendRestLen_ argLen buffLen =
                                      argLen - AppendWriteLen_ argLen buffLen

type family AppendNewBuffLen_ (argLen :: Nat) (buffLen :: Nat) ::
     Nat where
        AppendNewBuffLen_ argLen buffLen =
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
