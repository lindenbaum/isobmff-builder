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
    , BitString(..)
    , FiniteBitString(..)
    ) where

import           Data.Proxy
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


-- | A type class for types that store an exact known number of bits.
class BitString a where
    takeFirstBitBuffer :: a -> BitBuffer
    takeFirstByte :: a -> Word8
    prependZeros :: Int -> a -> a
    takeFirstBits :: Int -> a -> a
    dropFirstBits :: Int -> a -> a
    dropLastBits :: Int -> a -> a
    bitStringLength :: a -> Int
    isBitStringEmpty :: a -> Bool
    bitString :: (Integral x) => x -> Int -> a
    unsafeBitString :: BitBuffer -> Int -> a
    emptyBitString :: a

-- | A type class for types that store an exact known number of bits, but never
-- more than a fixed, finite number.
class BitString a =>
      FiniteBitString a where
    bitStringMaxLength :: proxy a -> Int
    bitStringMaxLength _ = bitBufferSize
    bitStringSpaceLeft :: a -> Int
    bitStringSpaceLeft x = bitStringMaxLength (Proxy :: Proxy a) -
        bitStringLength x
    -- | Like 'bufferBits' but work on an 'Integer' as bit source. This makes
    -- writing more the larger number of bits than 'BitBufferAlignment' more
    -- efficient, than 'bufferBits', because the input and rest bits don't need to
    -- be constantly converted with 'fromIntegral'.
    appendBitStrings :: (BitString arg, FiniteBitString a)
                     => arg -- ^ The value to write (in the lower @length@ bits).
                     -> a   -- ^ The 'BitString' to append to
                     -> (arg, a) -- ^ The remaining bits that did not fit in the buffer and the
                                   -- output string.

-- * 'BitString' instances

instance BitString BittrBuffer where
    takeFirstBitBuffer (BittrBuffer !c _) =
        c
    takeFirstByte (BittrBuffer !c _) =
        fromIntegral (unBitBuffer c)
    prependZeros !n (BittrBuffer !c !l) =
        BittrBuffer (c `unsafeShiftL` n) (max bitBufferSize (n + l))
    takeFirstBits !n (BittrBuffer !c !l) =
        BittrBuffer c' n'
      where
        !c' = (c `unsafeShiftL` s) `unsafeShiftR` s
        !s = bitBufferSize - n
        !n' = min n l
    dropFirstBits !n (BittrBuffer !c !l) =
        BittrBuffer c' l'
      where
        !c' = c `unsafeShiftR` n
        !l' = if n >= l then 0 else l - n
    dropLastBits !n (BittrBuffer !c !l) =
        BittrBuffer c' l'
      where
        !c' = (c `unsafeShiftL` s) `unsafeShiftR` s
        !s = bitBufferSize - l'
        !l' = if n >= l then 0 else l - n
    bitStringLength (BittrBuffer _ !l) =
        l
    isBitStringEmpty (BittrBuffer _ !l) =
        l == 0
    bitString = bittrBuffer . fromIntegral
    unsafeBitString = BittrBuffer
    emptyBitString = emptyBittrBuffer

instance BitString BitOutBuffer where
    takeFirstBitBuffer (BitOutBuffer !c _) =
        c
    takeFirstByte (BitOutBuffer !c _) =
        fromIntegral (unBitBuffer c `unsafeShiftR` (bitBufferSize - 8))
    prependZeros !n (BitOutBuffer !c !l) =
        BitOutBuffer (c `unsafeShiftR` n) (max bitBufferSize (n + l))
    takeFirstBits !n = dropLastBits (bitBufferSize - n)
    dropFirstBits !n (BitOutBuffer !c !l) =
        BitOutBuffer c' l'
      where
        !c' = c `unsafeShiftL` n
        !l' = if n >= l then 0 else l - n
    dropLastBits !n (BitOutBuffer !c !l) =
        BitOutBuffer c' l'
      where
        !c' = (c `unsafeShiftR` s) `unsafeShiftL` s
        !s = bitBufferSize - l'
        !l' = if n >= l then 0 else l - n
    bitStringLength (BitOutBuffer _ !l) =
        l
    isBitStringEmpty (BitOutBuffer _ !l) =
        l == 0
    bitString = bitOutBuffer . fromIntegral
    unsafeBitString = BitOutBuffer
    emptyBitString = emptyBitOutBuffer

instance FiniteBitString BitOutBuffer where
    bitStringSpaceLeft (BitOutBuffer _ !l) =
        bitBufferSize - l
    -- appendBitStrings !arg (BitOutBuffer !buff !offset) =
    --    let !spaceAvailable = bitBufferSize - offset
    --        !writeLen = min spaceAvailable len
    --        !len = bitStringLength arg
    --        !bits = takeFirstBitBuffer arg
    --        !writeOffset = spaceAvailable - writeLen
    --        !restLen = len - writeLen
    --        !restBits = bits .&. (1 `unsafeShiftL` restLen - 1)
    --        !buff' = buff .|.
    --            (bits `unsafeShiftR` restLen `unsafeShiftL` writeOffset)
    --    in
    --        ( unsafeBitString restBits restLen
    --        , BitOutBuffer buff' (offset + writeLen)
    --        )

    appendBitStrings !arg (BitOutBuffer !buffBits !offset) =
        let !spaceAvailable = bitBufferSize - offset
            !len = bitStringLength arg
            !writeArg = prependZeros writeOffset (dropFirstBits restLen arg)
            !writeLen = min spaceAvailable len
            !writeOffset = spaceAvailable - writeLen
            !restLen = len - writeLen
            !rest = dropLastBits writeLen arg
            !buffBits' = buffBits .|. takeFirstBitBuffer writeArg
        in
            ( rest , BitOutBuffer buffBits' (offset + writeLen) )

instance BitString BittrBufferUnlimited where
    takeFirstBitBuffer (BittrBufferUnlimited !c _) =
        fromIntegral c
    takeFirstByte (BittrBufferUnlimited !c _) =
        fromIntegral c
    prependZeros !n (BittrBufferUnlimited !c !l) =
        BittrBufferUnlimited (c `unsafeShiftL` n) (l + n)
    takeFirstBits !n (BittrBufferUnlimited !c !l) =
        BittrBufferUnlimited (c .&. (2 ^ n' - 1)) n'
      where
        !n' = min n l
    dropFirstBits !n (BittrBufferUnlimited !c !l) =
        BittrBufferUnlimited c' l'
      where
        !c' = c `unsafeShiftR` n
        !l' = if n >= l then 0 else l - n
    dropLastBits !n (BittrBufferUnlimited !c !l) =
        BittrBufferUnlimited c' l'
      where
        !c' = (c `unsafeShiftL` s) `unsafeShiftR` s
        !s = bitBufferSize - l'
        !l' = if n >= l then 0 else l - n
    bitStringLength (BittrBufferUnlimited _ !l) =
        l
    isBitStringEmpty (BittrBufferUnlimited _ !l) =
        l == 0
    bitString = bittrBufferUnlimited . fromIntegral
    unsafeBitString !b !l = BittrBufferUnlimited (fromIntegral (unBitBuffer b))
                                                 l
    emptyBitString = emptyBittrBufferUnlimited
