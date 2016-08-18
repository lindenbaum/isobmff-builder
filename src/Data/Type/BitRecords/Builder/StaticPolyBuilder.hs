{-# LANGUAGE UndecidableInstances #-}
module Data.Type.BitRecords.Builder.StaticPolyBuilder where

import Data.Type.BitRecords.Builder.Alignment
import Data.Type.BitRecords.Builder.BitBuffer
import Data.Type.BitRecords.Builder.Holey
import Data.Type.BitRecords.Core
import Data.Bits
import Data.Proxy
import Data.Monoid
import Control.Category
import GHC.TypeLits
import Text.Printf
import Prelude hiding ((.), id)
import Data.Tagged

import Data.Type.BitRecords.Builder.Poly
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Builder

formatBits
  :: forall rec off
   . ( off ~ GetRemainingUnaligned (GetRecordSize rec) 'Align64
     , ToHoley (BitBuilder 0 off) (Proxy rec) (BitBuilder 0 off))
  => Proxy rec
  -> ToM (BitBuilder 0 off) (Proxy rec) (BitBuilder 0 off)
formatBits pRec = runHoley toHoley'
  where
    toHoley' ::
      Holey
        (BitBuilder 0 off)
        (BitBuilder 0 off)
        (ToM (BitBuilder 0 off) (Proxy rec) (BitBuilder 0 off))
    toHoley' = toHoley pRec

toBuilder :: (KnownNat off, HasBuilder, Bits (ToAlignedWord 'Align64), Num (BitBuffer))
  => BitBuilder 0 off -> Builder
toBuilder !bb = appBitBuilder mempty (bb `ixAppend` flushBuilder)
  where
    flushBuilder :: forall off.
                 (KnownNat off)
                 => BitBuilder off 0
    flushBuilder = modifyBitBuilder flushBBState
      where
        flushBBState :: BBState off -> BBState 0
        flushBBState bb'@(BBState bldr part) =
            let !off = natVal bb'
                writeRestBytes !part' !off' !bldr' =
                    if off' == 0
                    then bldr'
                    else writeRestBytes (part' `unsafeShiftR` 8)
                                        (max 0 (off' - 8))
                                        (bldr' <> toByteBuilder part')
            in
                initialBBState $ writeRestBytes part off bldr

appBitBuilder :: Num (BitBuffer) => Builder -> BitBuilder 0 0 -> Builder
appBitBuilder !b (BitBuilder !f) =
  bbStateBuilder (appIxEndo f (initialBBState b))

startBitBuilder :: Num (BitBuffer) => Builder -> BitBuilder 0 0
startBitBuilder !b = modifyBitBuilder (const (initialBBState b))

newtype BitBuilder (fromOffset :: Nat)
                   (toOffset   :: Nat) =
    BitBuilder (IxEndo BBState fromOffset toOffset)
  deriving IxMonoid

modifyBitBuilder
  :: (BBState fromOffset -> BBState toOffset)
  -> BitBuilder fromOffset toOffset
modifyBitBuilder = BitBuilder . IxEndo


data BBState (offset :: Nat) =
  BBState {  bbStateBuilder    :: !Builder
          , _bbStatePart       :: !BitBuffer }

instance (KnownNat o, Show (BitBuffer)) => Show (BBState o) where
  showsPrec d st@(BBState b p) =
    showParen (d > 10) $
          showString (printf "BBState %s" (printBuilder b))
        . showChar ' '
        . showsPrec 11 p
        . showChar ' '
        . showsPrec 11 (natVal st)


initialBBState :: Num BitBuffer => Builder -> BBState 0
initialBBState b = BBState b 0


-- | Write all the bits, in chunks, filling and writing the 'BitBuffer'
-- in the 'BitBuilder' as often as necessary.
writeBits
      :: ( KnownNat len
         , KnownNat fromOffset
         , buff ~ BitBuffer
         , HasBuilder
         , KnownNat toOffset
         , toOffset ~ AlignmentOffsetAdd 'Align64 len fromOffset)
      => proxy (len :: Nat) -- TODO add a len to BitBuffer, then remove this
      -> BitBuffer
      -> BitBuilder fromOffset toOffset
writeBits !pLen !pBits =
    modifyBitBuilder $
        \bb@(BBState !builder !part) ->
            let pLenVal = fromIntegral (natVal pLen)
                offset = fromIntegral (natVal bb)
            in
                go (bittrBuffer pBits pLenVal) builder (bittrBuffer part offset)
  where
    go !arg !builder !buff
        | isBittrBufferEmpty arg = BBState builder (bittrBufferContent buff)
        | otherwise = let (arg', buff') = bufferBits arg buff
                      in
                          if bittrBufferSpaceLeft buff' > 0
                          then BBState builder (bittrBufferContent buff')
                          else let builder' = builder <>  toBitBufferBuilder (bittrBufferContent buff')
                               in  go arg' builder' buff'

-------------------------

instance ( KnownNat oF, KnownNat oT, HasBuilder
         , KnownNat (GetRecordSize f)
         , oT ~ AlignmentOffsetAdd 'Align64 (GetRecordSize f) oF)
  => ToHoley (BitBuilder oF oT) (Proxy (l :=> f)) r where
    type ToM (BitBuilder oF oT) (Proxy (l :=> f)) r =
      Tagged l Integer -> r
    toHoley _ =
        indirect (writeBits fieldLen . fromIntegral)
      where
        fieldLen = Proxy :: Proxy (GetRecordSize f)

instance  ( HasBuilder
          , KnownNat oF, KnownNat oT
          , KnownNat v
          , KnownNat (GetRecordSize f)
          , oT ~ AlignmentOffsetAdd 'Align64 (GetRecordSize f) oF)
  => ToHoley (BitBuilder oF oT) (Proxy (f := v)) r where
    toHoley _ =
        immediate (writeBits fieldLen fieldVal)
      where
        fieldLen = Proxy :: Proxy (GetRecordSize f)
        fieldVal = fromIntegral (natVal (Proxy :: Proxy v))

instance forall oT n oF r .
          ( HasBuilder
          , KnownNat n
          , KnownNat oF
          , oT ~ AlignmentOffsetAdd 'Align64 n oF
          , KnownNat oT)
  => ToHoley (BitBuilder oF oT) (Proxy (Field n)) r where
    toHoley _ = immediate (writeBits (Proxy :: Proxy n) 0)
-- TODO
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
-- runHoley $ toHoley (Proxy :: Proxy TwoFields) 1 2
-- @
--
-- Will result in:
-- @       MSB                                             LSB
--    Bit: |k  ..  k-(m+1)|k-m  ..  k-(m+n+1)| k-(m+n)  ..  0|
--  Value: |0     ..     1|0       ..      10| X    ..      X|
-- @
instance forall f0 f1 toM oF oT .
         ( ToHoley (BitBuilder oF (AlignmentOffsetAdd 'Align64 (GetRecordSize f0) oF)) (Proxy f0) (ToM (BitBuilder (AlignmentOffsetAdd 'Align64 (GetRecordSize f0) oF) oT) (Proxy f1) toM)
         , ToHoley (BitBuilder (AlignmentOffsetAdd 'Align64 (GetRecordSize f0) oF) oT) (Proxy f1) toM
         , oT ~ (AlignmentOffsetAdd 'Align64 (GetRecordSize f1) (AlignmentOffsetAdd 'Align64 (GetRecordSize f0) oF))
         , KnownNat oF
         , KnownNat (AlignmentOffsetAdd 'Align64 (GetRecordSize f0) oF)
         , KnownNat oT
         , HasBuilder)
  => ToHoley (BitBuilder oF oT) (Proxy (f0 :>: f1)) toM where
    type ToM (BitBuilder oF oT) (Proxy (f0 :>: f1)) toM =
      ToM
        (BitBuilder oF (AlignmentOffsetAdd 'Align64 (GetRecordSize f0) oF))
        (Proxy f0)
        (ToM
          (BitBuilder (AlignmentOffsetAdd 'Align64 (GetRecordSize f0) oF) oT)
          (Proxy f1)
          toM)
    toHoley _ = fmt0 % fmt1
      where
        fmt0 :: Holey -- rely on ScopedTypeVariables and apply the types
                      -- so the compiler knows the result type of
                      -- toHoley. Only then 'o' and
                      -- 'c ~ (ToM (BitBuilder oF oT) (f0 :>: f1) toM)'
                      -- is known, yeah figure 'c' out ;)
                 (BitBuilder oF (AlignmentOffsetAdd 'Align64 (GetRecordSize f0) oF))
                 (ToM (BitBuilder (AlignmentOffsetAdd 'Align64 (GetRecordSize f0) oF) oT) (Proxy f1) toM)
                 (ToM (BitBuilder oF oT) (Proxy (f0 :>: f1)) toM)
        fmt0 = toHoley pf0
        fmt1 = toHoley pf1
        pf0 = Proxy :: Proxy f0
        pf1 = Proxy :: Proxy f1

-------------------------------------------------------------
