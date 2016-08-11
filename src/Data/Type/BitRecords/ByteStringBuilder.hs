{-# LANGUAGE UndecidableInstances #-}
module Data.Type.BitRecords.ByteStringBuilder where
  -- TODO

import Data.Type.BitRecords.Builder.Alignment
import Data.Type.BitRecords.Builder.BitBuffer
import Data.Type.BitRecords.Builder.Parameter
import Data.Type.BitRecords.Core
import Data.Bits
import Data.Proxy
import Data.Monoid
import Data.ByteString.Builder
import Control.Category
import GHC.TypeLits
import Text.Printf
import qualified Data.ByteString.Lazy as B
import Prelude hiding ((.), id)
import Data.Tagged

-- | Words acting as aligned bit buffers, that can eventually be converted to a
-- 'Builder'.
class HasBuilder a where
  wordBuilder :: a -> Builder

instance HasBuilder (BitBuffer 'Align64 'LittleEndian) where
  wordBuilder = word64LE . fromBitBufferMsbFirst
instance HasBuilder (BitBuffer 'Align64 'BigEndian)    where
  wordBuilder = word64BE . fromBitBufferMsbFirst
instance HasBuilder (BitBuffer 'Align32 'LittleEndian) where
  wordBuilder = word32LE . fromBitBufferMsbFirst
instance HasBuilder (BitBuffer 'Align32 'BigEndian)    where
  wordBuilder = word32BE . fromBitBufferMsbFirst
instance HasBuilder (BitBuffer 'Align16 'LittleEndian) where
  wordBuilder = word16LE . fromBitBufferMsbFirst
instance HasBuilder (BitBuffer 'Align16 'BigEndian)    where
  wordBuilder = word16BE . fromBitBufferMsbFirst
instance HasBuilder (BitBuffer 'Align8 e) where
  wordBuilder = word8    . fromBitBufferMsbFirst

------------------

formatBits
  :: forall proxy0 proxy1 proxy2 rec endianness buff alignment off
   . ( buff ~ BitBuffer alignment endianness
     , off ~ GetRemainingUnaligned (GetRecordSize rec) alignment
     , HasParameter (BitBuilder buff 0 off) rec (BitBuilder buff 0 off))
  => proxy0 endianness
  -> proxy1 alignment
  -> proxy2 rec
  -> FmtArg (BitBuilder buff 0 off) rec (BitBuilder buff 0 off)
formatBits _pEnd _pAlign pRec = runHoley toFormatter'
  where
    toFormatter' ::
      Holey
        (BitBuilder buff 0 off)
        (BitBuilder buff 0 off)
        (FmtArg (BitBuilder buff 0 off) rec (BitBuilder buff 0 off))
    toFormatter' = toFormatter pRec

formatAlignedBits
  :: forall proxy0 proxy1 rec endianness buff alignment recSize off
   . ( 'Just alignment ~ SelectAlignment (GetRecordSize rec)
     , buff    ~ BitBuffer alignment endianness
     , FiniteBits (ToAlignedWord alignment)
     , HasBuilder buff
     , recSize ~ GetRecordSize rec
     , KnownNat recSize
     , off ~ GetRemainingUnaligned recSize alignment
     , KnownNat off
     , HasParameter (BitBuilder buff 0 off) rec (BitBuilder buff 0 off))
  => proxy0 endianness
  -> proxy1 rec
  -> FmtArg (BitBuilder buff 0 off) rec (BitBuilder buff 0 off)
formatAlignedBits _pEnd pRec = runHoley toFormatter'
  where
    toFormatter' ::
      Holey
        (BitBuilder buff 0 off)
        (BitBuilder buff 0 off)
        (FmtArg (BitBuilder buff 0 off) rec (BitBuilder buff 0 off))
    toFormatter' = toFormatter pRec

toBuilder :: Num buff => BitBuilder buff 0 0 -> Builder
toBuilder = appBitBuilder mempty

toFlushedBuilder :: (KnownNat off, Num buff, HasBuilder buff)
  => BitBuilder buff 0 off -> Builder
toFlushedBuilder bb = toBuilder (bb `ixAppend` flushBuilder)

flushBuilder :: forall buff off . (KnownNat off, Num buff, HasBuilder buff)
  => BitBuilder buff off 0
flushBuilder =
    let flushBBState :: BBState buff off -> BBState buff 0
        flushBBState bb@(BBState bldr part) =
          let off = natVal bb
          in initialBBState $
              if off == 0
                then bldr
                else bldr <> wordBuilder part
    in  modifyBitBuilder flushBBState

appBitBuilder :: Num buff => Builder -> BitBuilder buff 0 0 -> Builder
appBitBuilder !b (BitBuilder !f) =
  bbStateBuilder (appIxEndo f (initialBBState b))

startBitBuilder :: Num buff => Builder -> BitBuilder buff n 0
startBitBuilder b = modifyBitBuilder (const (initialBBState b))

newtype BitBuilder buff
                   (fromOffset :: Nat)
                   (toOffset :: Nat)
  = BitBuilder (IxEndo (BBState buff) fromOffset toOffset)
  deriving IxMonoid

modifyBitBuilder
  :: (BBState buff fromOffset -> BBState buff toOffset)
  -> BitBuilder buff fromOffset toOffset
modifyBitBuilder = BitBuilder . IxEndo

data BBState buff (offset :: Nat) =
  BBState {  bbStateBuilder    :: !Builder
          , _bbStatePart       :: !buff}

instance (KnownNat o, Show buff) => Show (BBState buff o) where
  showsPrec d st@(BBState b p) =
    showParen (d > 10) $
          showString (printf "BBState %s" (printBitBuffer b))
        . (showChar ' ')
        . (showsPrec 11 p)
        . (showChar ' ')
        . (showsPrec 11 (natVal st))

printBitBuffer :: Builder -> String
printBitBuffer b =
      ("<< " ++)
   $  (++" >>")
   $  unwords
   $  printf "%0.2x"
  <$> (B.unpack $ toLazyByteString b)


initialBBState :: Num buff => Builder -> BBState buff 0
initialBBState b = BBState b 0


-- | Write all the bits, in chunks, filling and writing the 'BitBuffer'
-- in the 'BitBuilder' as often as necessary.
writeBits
      :: ( KnownNat len, HasBuilder buff
         , KnownNat fromOffset
         , buff ~ BitBuffer a e
         , IsBitBuffer a e
         , KnownNat toOffset
         , toOffset ~ NextBitBufferOffset buff len fromOffset)
      => proxy (len :: Nat)
      -> BitBuffer a e
      -> BitBuilder (BitBuffer a e) fromOffset toOffset
writeBits pLen !pBits =
  modifyBitBuilder $
    \bb@(BBState !bldr !part) ->
      let pLenVal = fromIntegral (natVal pLen)
          maskedBits = let mask = (1 `unsafeShiftL` pLenVal) - 1
                           in pBits .&. mask
          offset = fromIntegral (natVal bb)
          in go pLenVal maskedBits bldr part offset
  where
    go 0 _bits !bldr !part _ =  BBState bldr part
    go !len !bits !builder !part !offset =
      let (part', spaceLeft, restLen, restBits) = bufferBits len bits offset part
          in if spaceLeft > 0
                then BBState builder part'
                else let nextBuilder = builder <> wordBuilder part'
                         in go restLen restBits nextBuilder 0 0

-------------------------
instance ( KnownNat oF, KnownNat oT, HasBuilder (BitBuffer a e)
         , IsBitBuffer a e, KnownNat (GetRecordSize f)
         , oT ~ NextBitBufferOffset (BitBuffer a e) (GetRecordSize f) oF)
  => HasParameter (BitBuilder (BitBuffer a e) oF oT) (l :=> f) r where
    type FmtArg (BitBuilder (BitBuffer a e) oF oT) (l :=> f) r =
      Tagged l Integer -> r
    toFormatter _ =
        indirect (writeBits fieldLen . fromIntegral)
      where
        fieldLen = Proxy :: Proxy (GetRecordSize f)

instance  ( HasBuilder (BitBuffer align endian)
          , KnownNat oF, KnownNat oT
          , IsBitBuffer align endian
          , KnownNat v
          , KnownNat (GetRecordSize f)
          , oT ~ NextBitBufferOffset (BitBuffer align endian) (GetRecordSize f) oF)
  => HasParameter (BitBuilder (BitBuffer align endian) oF oT) (f := v) r where
    type FmtArg (BitBuilder (BitBuffer align endian) oF oT) (f := v) r = r
    toFormatter _ =
        immediate (writeBits fieldLen fieldVal)
      where
        fieldLen = Proxy :: Proxy (GetRecordSize f)
        fieldVal = fromIntegral (natVal (Proxy :: Proxy v))

-- | An instance that when given:
--
-- > type TwoFields = "f0" :=> Field m :>: "f1" :=> Field n
--
-- Writes:
-- @       MSB                                             LSB
--    Bit: |k  ..  k-(m+1)|k-m  ..  k-(m+n+1)| k-(m+n)  ..  0|
--  Value: \------f0-----/\--------f1--------/\--- empty ---/
-- @
--
-- Where @k@ is the current bit offset.
-- The input values are expected to be in the order of the fields, i.e.:
--
-- @
-- runHoley $ toFormatter (Proxy :: Proxy TwoFields) 1 2
-- @
--
-- Will result in:
-- @       MSB                                             LSB
--    Bit: |k  ..  k-(m+1)|k-m  ..  k-(m+n+1)| k-(m+n)  ..  0|
--  Value: |0     ..     1|0       ..      10| X    ..      X|
-- @
instance forall f0 f1 a oF oT align endian .
         ( HasParameter (BitBuilder (BitBuffer align endian) oF (NextBitBufferOffset (BitBuffer align endian) (GetRecordSize f0) oF)) f0 (FmtArg (BitBuilder (BitBuffer align endian) (NextBitBufferOffset (BitBuffer align endian) (GetRecordSize f0) oF) oT) f1 a)
         , HasParameter (BitBuilder (BitBuffer align endian) (NextBitBufferOffset (BitBuffer align endian) (GetRecordSize f0) oF) oT) f1 a
         , oT ~ (NextBitBufferOffset (BitBuffer align endian) (GetRecordSize f1) (NextBitBufferOffset (BitBuffer align endian) (GetRecordSize f0) oF))
         , KnownNat oF, KnownNat (NextBitBufferOffset (BitBuffer align endian) (GetRecordSize f0) oF), KnownNat oT
         , IsBitBuffer align endian
         , HasBuilder (BitBuffer align endian))
  => HasParameter (BitBuilder (BitBuffer align endian) oF oT) (f0 :>: f1) a where
    type FmtArg (BitBuilder (BitBuffer align endian) oF oT) (f0 :>: f1) a =
      FmtArg
        (BitBuilder (BitBuffer align endian) oF (NextBitBufferOffset (BitBuffer align endian) (GetRecordSize f0) oF))
        f0
        (FmtArg
          (BitBuilder (BitBuffer align endian) (NextBitBufferOffset (BitBuffer align endian) (GetRecordSize f0) oF) oT)
          f1
          a)
    toFormatter _ = fmt0 % fmt1
      where
        fmt0 :: Holey -- rely on ScopedTypeVariables and apply the types
                      -- so the compiler knows the result type of
                      -- toFormatter. Only then 'o' and
                      -- 'c ~ (FmtArg (BitBuilder (BitBuffer align endian) oF oT) (f0 :>: f1) a)'
                      -- is known, yeah figure 'c' out ;)
                 (BitBuilder (BitBuffer align endian) oF (NextBitBufferOffset (BitBuffer align endian) (GetRecordSize f0) oF))
                 (FmtArg (BitBuilder (BitBuffer align endian) (NextBitBufferOffset (BitBuffer align endian) (GetRecordSize f0) oF) oT) f1 a)
                 (FmtArg (BitBuilder (BitBuffer align endian) oF oT) (f0 :>: f1) a)
        fmt0 = toFormatter pf0
        fmt1 = toFormatter pf1
        pf0 = Proxy :: Proxy f0
        pf1 = Proxy :: Proxy f1

-------------------------------------------------------------
