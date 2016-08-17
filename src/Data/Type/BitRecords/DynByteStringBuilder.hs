{-# LANGUAGE UndecidableInstances #-}
module Data.Type.BitRecords.DynByteStringBuilder where

import           Data.Type.BitRecords.Builder.Alignment
import           Data.Type.BitRecords.Builder.BitBuffer
import           Data.Type.BitRecords.Builder.Holey
import           Data.Type.BitRecords.ByteStringBuilder
import           Data.Type.BitRecords.Core
import           Data.Word

import           Data.Bits
import           Data.Proxy
import           Data.Monoid
import           Data.ByteString.Builder
import           Control.Category
import           GHC.TypeLits
import           Text.Printf
import qualified Data.ByteString.Lazy as B
import           Prelude hiding ((.), id)
import           Data.Tagged

----------------
----------------
----------------

newtype BittrWriter = BittrWriter { unBittrWriter :: Endo BittrWriterState }
  deriving Monoid

runBittrWriter :: BittrWriter -> Builder
runBittrWriter !w =
  evalBittrWriterState $ appBittrWriter w initialBittrWriterState

appBittrWriter :: BittrWriter -> BittrWriterState -> BittrWriterState
appBittrWriter !w = appEndo (unBittrWriter w)

data BittrWriterState = BittrWriterState !Builder !BittrBuffer

initialBittrWriterState :: BittrWriterState
initialBittrWriterState = BittrWriterState mempty (BittrBuffer 0 0)

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

-- | Content of unrestricted length.
data BittrBufferUnlimited =
  -- | Parameters are the content as well as the number of bits from the
  -- content.
  BittrBufferUnlimited
    !Integer
    !Int

-- | Write all the bits, in chunks, filling and writing the 'BittrBuffer'
-- in the 'BittrWriterState' as often as necessary.
appendUnlimited :: BittrBufferUnlimited -> BittrWriter
appendUnlimited (BittrBufferUnlimited !allBits !totalLen) =
  BittrWriter $
  Endo $
    \(BittrWriterState !builder (BittrBuffer !part !offset)) ->
      let !maskBittrs = allBits .&. mask
          !mask = (1 `unsafeShiftL` totalLen) - 1
      in go totalLen maskBittrs builder part offset
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


-- | Write all the bits, in chunks, filling and writing the 'BittrBuffer'
-- in the 'BittrWriterState' as often as necessary.
appendBittrBuffer :: BittrBuffer -> BittrWriter
appendBittrBuffer (BittrBuffer !allBits !totalLen) =
  BittrWriter $
  Endo $
    \(BittrWriterState !builder (BittrBuffer !part !offset)) ->
      let !maskBittrs = allBits .&. mask
          !mask = (1 `unsafeShiftL` totalLen) - 1
      in go totalLen maskBittrs builder part offset
  where
    go !len !bits !builder !part !offset
      | len == 0 = BittrWriterState builder (BittrBuffer part offset)
      | otherwise =
          let (!part', !spaceLeft, !restLen, !rest) =
                bufferBits len bits offset part
          in if spaceLeft > 0
                then
                  let !offset' = offset + len
                      in BittrWriterState builder (BittrBuffer part' offset')
                else
                  let !nextBuilder = builder <> toBitBufferBuilder part'
                      in go restLen rest nextBuilder 0 0


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

class HasBittrWriter x result where
  type ToBittrWriter x result
  type ToBittrWriter x result = result
  getBittrWriterHoley
    :: x -> Holey BittrWriter result (ToBittrWriter x result)

getAndRunBittrWriterHoley
        :: (HasBittrWriter x Builder)
  => x -> ToBittrWriter x Builder
getAndRunBittrWriterHoley x =
  runHoley
    (hoistM (evalBittrWriterState . flip appBittrWriter initialBittrWriterState)
                    (getBittrWriterHoley x))

runBittrWriterHoley
  :: Holey BittrWriter Builder a -> a
runBittrWriterHoley (HM !x) = x runBittrWriter


instance HasBittrWriter BittrBufferUnlimited r where
  getBittrWriterHoley = immediate . appendUnlimited

instance HasBittrWriter BittrBuffer r where
  getBittrWriterHoley = immediate . appendBittrBuffer
