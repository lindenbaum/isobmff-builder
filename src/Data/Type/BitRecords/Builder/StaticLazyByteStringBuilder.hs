{-# LANGUAGE UndecidableInstances #-}
module Data.Type.BitRecords.Builder.StaticLazyByteStringBuilder where

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
import Debug.Trace
import Data.Type.BitRecords.Builder.Poly
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Builder


newtype BittrWriter (fromOffset :: Nat)
                    (toOffset   :: Nat) =
    BittrWriter {unBittrWriter :: IxEndo BittrWriterState fromOffset toOffset}
  deriving IxMonoid

runBittrWriter :: (KnownNat off) => BittrWriter 0 off -> Builder
runBittrWriter !w = evalBittrWriterState $
    appBittrWriter (w `ixAppend` flushBuilder) initialBittrWriterState
  where
    flushBuilder :: forall off. (KnownNat off) => BittrWriter off 0
    flushBuilder = modifyBittrWriter flushBittrWriterState

-- | Write the partial buffer contents using  any number of 'word8'
--   The unwritten parts of the bittr buffer are at the top.
--   If the
--
-- >     63  ...  (63-off-1)(63-off)  ...  0
-- >     ^^^^^^^^^^^^^^^^^^^
-- > Relevant bits start to the top!
--
flushBittrWriterState :: (KnownNat off) => BittrWriterState off -> BittrWriterState 0
flushBittrWriterState bb@(BittrWriterState bldr part) =
    trace "Flush" $
        traceShow part $
            let !off = fromIntegral $ natVal bb
                -- write bytes from msb to lsb until the offset is reached
                -- >  63  ...  (63-off-1)(63-off)  ...  0
                -- >  ^^^^^^^^^^^^^^^^^^^
                -- >  AAAAAAAABBBBBBBBCCC00000
                -- >  |byte A| byte B| byte C|
                writeRestBytes !bldr' !flushOffset =
                    if off <= flushOffset
                    then bldr'
                    else let !flushOffset' = flushOffset + 8
                             !bldr'' = bldr' <>
                                 toByteBuilder (traceShow ( (bitBufferSize -
                                                                 flushOffset')
                                                          , off
                                                          ) $
                                                    traceShowId $
                                                        (part `unsafeShiftR`
                                                             (bitBufferSize -
                                                                  flushOffset')) .&.
                                                            0xFF)
                         in
                             writeRestBytes bldr'' flushOffset'
            in
                BittrWriterState (writeRestBytes bldr 0) 0

appBittrWriter :: BittrWriter from to -> BittrWriterState from -> BittrWriterState to
appBittrWriter !w = appIxEndo (unBittrWriter w)

-- startBittrWriter :: Builder -> BittrWriter 0 0
-- startBittrWriter !b = modifyBittrWriter (const (initialBittrWriterState b))

modifyBittrWriter
  :: (BittrWriterState fromOffset -> BittrWriterState toOffset)
  -> BittrWriter fromOffset toOffset
modifyBittrWriter = BittrWriter . IxEndo


data BittrWriterState (offset :: Nat) =
      BittrWriterState { bbStateBuilder :: !Builder
                       , _bbStatePart   :: !BitBuffer
                       }

initialBittrWriterState :: BittrWriterState 0
initialBittrWriterState = BittrWriterState mempty 0

evalBittrWriterState :: BittrWriterState 0 -> Builder
evalBittrWriterState (BittrWriterState !builder _) = builder

--  printBuilder (runBittrWriterHoley (toHoley (Proxy :: Proxy (Field 8 := 1 :>: Field 8 := 0 :>: Field 7 := 3 :>: Field 32 := 0 :>: Field 8 := 7 :>: Field 8 := 0xfe ))))
-- | Write all the bits, in chunks, filling and writing the 'BitBuffer'
-- in the 'BittrWriter' as often as necessary.
writeBits
      :: ( KnownNat len
         , KnownNat fromOffset
         , buff ~ BitBuffer
         , HasBuilder
         , KnownNat toOffset
         , toOffset ~ AlignmentOffsetAdd 'Align64 len fromOffset)
      => proxy (len :: Nat) -- TODO add a len to BitBuffer, then remove this
      -> BitBuffer
      -> BittrWriter fromOffset toOffset
writeBits !pLen !pBits =
    modifyBittrWriter $
        \bb@(BittrWriterState !builder !part) ->
            let pLenVal = fromIntegral (natVal pLen)
                offset = fromIntegral (natVal bb)
            in
                go (bittrBuffer pBits pLenVal)
                   (trace (printf "writeBits. Appending to: %s  partial bits: %64b offset: %d, input data: %64b len: %d"
                                  (printBuilder builder)
                                  (unBitBuffer part)
                                  offset
                                  (unBitBuffer pBits)
                                  pLenVal) $
                        builder)
                   (bittrBuffer part offset)
  where
    go !arg !builder !buff
        | isBittrBufferEmpty arg =
              trace "Aligned" $
                  BittrWriterState builder (bittrBufferContent buff)
        | otherwise = let (arg', buff') = bufferBits arg buff
                      in
                          if bittrBufferSpaceLeft buff' > 0
                          then trace "Partially" $
                              BittrWriterState builder
                                               (bittrBufferContent buff')
                          else let builder' = builder <>
                                       toBitBufferBuilder (bittrBufferContent buff')
                               in
                                   trace "recurse" $
                                       go arg' builder' emptyBittrBuffer

-------------------------

runBittrWriterHoley :: KnownNat off => Holey (BittrWriter 0 off) Builder r -> r
runBittrWriterHoley (HM !x) = x runBittrWriter

instance ( KnownNat oF, KnownNat oT, HasBuilder
         , KnownNat (GetRecordSize f)
         , oT ~ AlignmentOffsetAdd 'Align64 (GetRecordSize f) oF)
  => ToHoley (BittrWriter oF oT) (Proxy (l :=> f)) r where
    type ToM (BittrWriter oF oT) (Proxy (l :=> f)) r =
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
  => ToHoley (BittrWriter oF oT) (Proxy (f := v)) r where
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
  => ToHoley (BittrWriter oF oT) (Proxy (Field n)) r where
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
         ( ToHoley (BittrWriter oF (AlignmentOffsetAdd 'Align64 (GetRecordSize f0) oF)) (Proxy f0) (ToM (BittrWriter (AlignmentOffsetAdd 'Align64 (GetRecordSize f0) oF) oT) (Proxy f1) toM)
         , ToHoley (BittrWriter (AlignmentOffsetAdd 'Align64 (GetRecordSize f0) oF) oT) (Proxy f1) toM
         , oT ~ (AlignmentOffsetAdd 'Align64 (GetRecordSize f1) (AlignmentOffsetAdd 'Align64 (GetRecordSize f0) oF))
         , KnownNat oF
         , KnownNat (AlignmentOffsetAdd 'Align64 (GetRecordSize f0) oF)
         , KnownNat oT
         , HasBuilder)
  => ToHoley (BittrWriter oF oT) (Proxy (f0 :>: f1)) toM where
    type ToM (BittrWriter oF oT) (Proxy (f0 :>: f1)) toM =
      ToM
        (BittrWriter oF (AlignmentOffsetAdd 'Align64 (GetRecordSize f0) oF))
        (Proxy f0)
        (ToM
          (BittrWriter (AlignmentOffsetAdd 'Align64 (GetRecordSize f0) oF) oT)
          (Proxy f1)
          toM)
    toHoley _ = fmt0 % fmt1
      where
        fmt0 :: Holey -- rely on ScopedTypeVariables and apply the types
                      -- so the compiler knows the result type of
                      -- toHoley. Only then 'o' and
                      -- 'c ~ (ToM (BittrWriter oF oT) (f0 :>: f1) toM)'
                      -- is known, yeah figure 'c' out ;)
                 (BittrWriter oF (AlignmentOffsetAdd 'Align64 (GetRecordSize f0) oF))
                 (ToM (BittrWriter (AlignmentOffsetAdd 'Align64 (GetRecordSize f0) oF) oT) (Proxy f1) toM)
                 (ToM (BittrWriter oF oT) (Proxy (f0 :>: f1)) toM)
        fmt0 = toHoley pf0
        fmt1 = toHoley pf1
        pf0 = Proxy :: Proxy f0
        pf1 = Proxy :: Proxy f1

-------------------------------------------------------------
