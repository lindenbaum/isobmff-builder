{-# LANGUAGE UndecidableInstances #-}
module Data.Type.BitRecords.DynByteStringBuilder where

import Data.Type.BitRecords.Builder.Alignment
import Data.Type.BitRecords.Builder.BitBuffer
import Data.Type.BitRecords.Builder.Holey
import Data.Type.BitRecords.Core
import Data.Type.BitRecords.ByteStringBuilder

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

-- toBittrWriterBuilder :: Holey BittrWriter r a
-- toBittrWriterBuilder =

----------------
----------------
----------------

newtype BittrWriter = BittrWriter { unBittrWriter :: Endo BittrWriterState }
  deriving Monoid

appBittrWriter :: BittrWriter -> BittrWriterState -> BittrWriterState
appBittrWriter = appEndo . unBittrWriter

-- runBittrWriter
--   :: (HasBittrWriter x Builder, HasBittrWriter x BittrWriterState)
--   => x -> ToBittrWriter x Builder
-- runBittrWriter = runHoley . xxx . getBittrWriterBuilder
--   where
--     xxx :: Holey t BittrWriterState a ->
--     xxx (HM f) = HM $ \k -> f (appBittrWriter mempty . k)

data BittrWriterState = BittrWriterState !Builder !BittrBuffer

evalBittrWriterState :: BittrWriterState -> Builder
evalBittrWriterState (BittrWriterState !builder (BittrBuffer !_rest !restLen)) =
    flushedBuilder
  where
    !flushedBuilder =
      if restLen > 0
        then error "TODO implement flush"
        else builder

data BittrBuffer = BittrBuffer !(BitBuffer 'Align64) !Int

---

type BittrWriterBuilder result toResult = Holey BittrWriter result toResult

runBittrWriterBuilder :: BittrWriterBuilder BittrWriter toResult -> toResult
runBittrWriterBuilder = runHoley

class HasBittrWriter x result where
  type ToBittrWriter x result
  type ToBittrWriter x result = result
  getBittrWriterBuilder
    :: x -> BittrWriterBuilder result (ToBittrWriter x result)

getAndRunBittrWriterBuilder
  :: HasBittrWriter x BittrWriter
  => x -> ToBittrWriter x BittrWriter
getAndRunBittrWriterBuilder = runBittrWriterBuilder . getBittrWriterBuilder

---

data BittrBufferUnlimited = BittrBufferUnlimited !Integer !Int

-- | Write all the bits, in chunks, filling and writing the 'BittrBuffer'
-- in the 'BittrWriterState' as often as necessary.
appendUnlimiteBittrs :: BittrBufferUnlimited -> BittrWriterBuilder r r
appendUnlimiteBittrs (BittrBufferUnlimited !allBits !totalLen) =
  immediate $
  BittrWriter $
  Endo $
    \(BittrWriterState !builder (BittrBuffer !part !offset)) ->
      let !maskeBittrs = allBits .&. mask
          !mask = (1 `unsafeShiftL` totalLen) - 1
      in go totalLen maskeBittrs builder part offset
  where
    go !len !bits !builder !part !offset
      | len == 0 = BittrWriterState builder (BittrBuffer part offset)
      | otherwise =
          let (!part', !spaceLeft, !restLen, !rest) =
                bufferBitsInteger len bits offset part
          in if spaceLeft > 0
                then
                  let !offset' = offset + len
                      in BittrWriterState builder (BittrBuffer part' offset')
                else
                  let !nextBuilder = builder <> toBitBufferBuilder part'
                      in go restLen rest nextBuilder 0 0


-- formatAligneBittrs
--   :: forall proxy0 rec alignment recSize off
--    . ( recSize ~ GetRecordSize rec
--      , KnownNat recSize
--      , off ~ GetRemainingUnaligned recSize alignment
--      , 'Just alignment ~ SelectAlignment (GetRecordSize rec)
--      , FiniteBits (ToAlignedWord alignment)
--      , HasBuilder alignment
--      , KnownNat off
--      , ToHoley (BitBuilder alignment 0 off) rec (BitBuilder alignment 0 off))
--   => proxy0 rec
--   -> ToM (BitBuilder alignment 0 off) rec (BitBuilder alignment 0 off)
-- formatAligneBittrs !pRec = runHoley toHoley'
--   where
--     toHoley' ::
--       Holey
--         (BitBuilder alignment 0 off)
--         (BitBuilder alignment 0 off)
--         (ToM (BitBuilder alignment 0 off) rec (BitBuilder alignment 0 off))
--     !toHoley' = toHoley pRec
--
-- toBuilder :: (Num (BitBuffer a)) => BitBuilder a 0 0 -> Builder
-- toBuilder = appBitBuilder mempty
--
-- toFlushedBuilder :: (KnownNat off, HasBuilder a, Num (BitBuffer a))
--   => BitBuilder a 0 off -> Builder
-- toFlushedBuilder !bb = toBuilder (bb `ixAppend` flushBuilder)
--
-- flushBuilder
--   :: forall a off . (KnownNat off, Num (BitBuffer a), HasBuilder a)
--   => BitBuilder a off 0
-- flushBuilder =
--     let flushBBState :: BBState a off -> BBState a 0
--         flushBBState !bb@(BBState bldr part) =
--           let !off = natVal bb
--           in initialBBState $
--               if off == 0
--                 then bldr
--                 else bldr <> toBitBufferBuilder part
--     in  modifyBitBuilder flushBBState
--
-- appBitBuilder :: Num (BitBuffer a) => Builder -> BitBuilder a 0 0 -> Builder
-- appBitBuilder !b (BitBuilder !f) =
--   bbStateBuilder (appIxEndo f (initialBBState b))
--
-- startBitBuilder :: Num (BitBuffer a) => Builder -> BitBuilder a 0 0
-- startBitBuilder !b = modifyBitBuilder (const (initialBBState b))
--
-- newtype BitBuilder (a          :: Alignment)
--                    (fromOffset :: Nat)
--                    (toOffset   :: Nat) =
--     BitBuilder (IxEndo (BBState a) fromOffset toOffset)
--   deriving IxMonoid
--
-- modifyBitBuilder
--   :: (BBState a fromOffset -> BBState a toOffset)
--   -> BitBuilder a fromOffset toOffset
-- modifyBitBuilder = BitBuilder . IxEndo
--
--
-- data BBState (a :: Alignment) (offset :: Nat) =
--   BBState {  bbStateBuilder    :: !Builder  -- TODO HasBuilder in BBState
--           , _bbStatePart       :: !(BitBuffer a)}
--
-- instance (KnownNat o, Show (BitBuffer a)) => Show (BBState a o) where
--   showsPrec d st@(BBState b p) =
--     showParen (d > 10) $
--           showString (printf "BBState %s" (printBitBuffer b))
--         . (showChar ' ')
--         . (showsPrec 11 p)
--         . (showChar ' ')
--         . (showsPrec 11 (natVal st))
--
-- printBitBuffer :: Builder -> String
-- printBitBuffer b =
--       ("<< " ++)
--    $  (++" >>")
--    $  unwords
--    $  printf "%0.2x"
--   <$> (B.unpack $ toLazyByteString b)
--
--
-- initialBBState :: Num (BitBuffer a) => Builder -> BBState a 0
-- initialBBState b = BBState b 0
--
--
-- -- | Write all the bits, in chunks, filling and writing the 'BitBuffer'
-- -- in the 'BitBuilder' as often as necessary.
-- dwriteBits
--       :: ( KnownNat len
--          , KnownNat fromOffset
--          , buff ~ BitBuffer a
--          , HasBuilder a
--          , IsBitBuffer (BitBuffer a)
--          , KnownNat toOffset
--          , toOffset ~ AlignmentOffsetAdd a len fromOffset)
--       => proxy (len :: Nat) -- TODO add a len to BitBuffer, then remove this
--       -> BitBuffer a
--       -> BitBuilder a fromOffset toOffset
-- writeBits !pLen !pBits =
--   modifyBitBuilder $
--     \bb@(BBState !bldr !part) ->
--       let pLenVal = fromIntegral (natVal pLen)
--           maskeBittrs = let mask = (1 `unsafeShiftL` pLenVal) - 1
--                            in pBits .&. mask
--           offset = fromIntegral (natVal bb)
--           in go pLenVal maskeBittrs bldr part offset
--   where
--     go 0 _bits !bldr !part _ =  BBState bldr part
--     go !len !bits !builder !part !offset =
--       let (part', spaceLeft, restLen, restBits) = bufferBits len bits offset part
--           in if spaceLeft > 0
--                 then BBState builder part'
--                 else let nextBuilder = builder <> toBitBufferBuilder part'
--                          in go restLen restBits nextBuilder 0 0
--
-- -------------------------
--
-- instance ( KnownNat oF, KnownNat oT, HasBuilder a
--          , IsBitBuffer (BitBuffer a)
--          , KnownNat (GetRecordSize f)
--          , oT ~ AlignmentOffsetAdd a (GetRecordSize f) oF)
--   => ToHoley (BitBuilder a oF oT) (l :=> f) r where
--     type ToM (BitBuilder a oF oT) (l :=> f) r =
--       Tagged l Integer -> r
--     toHoley _ =
--         indirect (writeBits fieldLen . fromIntegral)
--       where
--         fieldLen = Proxy :: Proxy (GetRecordSize f)
--
-- instance  ( HasBuilder a
--           , KnownNat oF, KnownNat oT
--           , IsBitBuffer (BitBuffer a)
--           , KnownNat v
--           , KnownNat (GetRecordSize f)
--           , oT ~ AlignmentOffsetAdd a (GetRecordSize f) oF)
--   => ToHoley (BitBuilder a oF oT) (f := v) r where
--     toHoley _ =
--         immediate (writeBits fieldLen fieldVal)
--       where
--         fieldLen = Proxy :: Proxy (GetRecordSize f)
--         fieldVal = fromIntegral (natVal (Proxy :: Proxy v))
--
-- instance forall a oT n oF r .
--           ( HasBuilder a
--           , KnownNat n
--           , KnownNat oF
--           , oT ~ AlignmentOffsetAdd a n oF
--           , KnownNat oT
--           , IsBitBuffer (BitBuffer a))
--   => ToHoley (BitBuilder a oF oT) (Field n) r where
--     toHoley _ = immediate (writeBits (Proxy :: Proxy n) 0)
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
-- instance forall f0 f1 toM oF oT a .
--          ( ToHoley (BitBuilder a oF (AlignmentOffsetAdd a (GetRecordSize f0) oF)) f0 (ToM (BitBuilder a (AlignmentOffsetAdd a (GetRecordSize f0) oF) oT) f1 toM)
--          , ToHoley (BitBuilder a (AlignmentOffsetAdd a (GetRecordSize f0) oF) oT) f1 toM
--          , oT ~ (AlignmentOffsetAdd a (GetRecordSize f1) (AlignmentOffsetAdd a (GetRecordSize f0) oF))
--          , KnownNat oF
--          , KnownNat (AlignmentOffsetAdd a (GetRecordSize f0) oF)
--          , KnownNat oT
--          , IsBitBuffer (BitBuffer a)
--          , HasBuilder a)
--   => ToHoley (BitBuilder a oF oT) (f0 :>: f1) toM where
--     type ToM (BitBuilder a oF oT) (f0 :>: f1) toM =
--       ToM
--         (BitBuilder a oF (AlignmentOffsetAdd a (GetRecordSize f0) oF))
--         f0
--         (ToM
--           (BitBuilder a (AlignmentOffsetAdd a (GetRecordSize f0) oF) oT)
--           f1
--           toM)
--     toHoley _ = fmt0 % fmt1
--       where
--         fmt0 :: Holey -- rely on ScopedTypeVariables and apply the types
--                       -- so the compiler knows the result type of
--                       -- toHoley. Only then 'o' and
--                       -- 'c ~ (ToM (BitBuilder a oF oT) (f0 :>: f1) toM)'
--                       -- is known, yeah figure 'c' out ;)
--                  (BitBuilder a oF (AlignmentOffsetAdd a (GetRecordSize f0) oF))
--                  (ToM (BitBuilder a (AlignmentOffsetAdd a (GetRecordSize f0) oF) oT) f1 toM)
--                  (ToM (BitBuilder a oF oT) (f0 :>: f1) toM)
--         fmt0 = toHoley pf0
--         fmt1 = toHoley pf1
--         pf0 = Proxy :: Proxy f0
--         pf1 = Proxy :: Proxy f1
--
-- -------------------------------------------------------------
