{-# LANGUAGE UndecidableInstances #-}
module Data.Type.BitRecords.Builder.LazyByteStringBuilder where

import           Data.Type.BitRecords.Builder.BitBuffer
import           Data.Type.BitRecords.Builder.Holey
import           Data.Type.BitRecords.Builder.Poly

import           Data.Bits
import           Data.Monoid
-- import           Data.ByteString.Builder
import           Control.Category
import           Prelude hiding ((.), id)

----------------
----------------
----------------

newtype BittrWriter b = BittrWriter { unBittrWriter :: Endo (BittrWriterState b) }
  deriving Monoid

runBittrWriter :: Monoid b => BittrWriter b -> b
runBittrWriter !w =
  evalBittrWriterState $ appBittrWriter w initialBittrWriterState

appBittrWriter :: BittrWriter b -> BittrWriterState b -> BittrWriterState b
appBittrWriter !w = appEndo (unBittrWriter w)

data BittrWriterState b where
    BittrWriterState :: !b -> !BittrBuffer -> BittrWriterState b

initialBittrWriterState :: Monoid b => BittrWriterState b
initialBittrWriterState = BittrWriterState mempty (BittrBuffer 0 0)

evalBittrWriterState :: BittrWriterState b -> b
evalBittrWriterState (BittrWriterState !builder (BittrBuffer !_rest !restLen)) =
    flushedBuilder
  where
    !flushedBuilder =
      if restLen > 0
        then error "TODO implement flush"
        else builder

data BittrBuffer = BittrBuffer !BitBuffer !Int

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
appendUnlimited :: (Monoid wi, ToBitBufferBuilder wi) => BittrBufferUnlimited -> BittrWriter wi
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
appendBittrBuffer :: (Monoid wi, ToBitBufferBuilder wi) => BittrBuffer -> BittrWriter wi
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

runBittrWriterHoley :: (Monoid wi) => Holey (BittrWriter wi) wi a -> a
runBittrWriterHoley (HM !x) = x runBittrWriter


instance (ToBitBufferBuilder wi,  Monoid wi) => ToHoley (BittrWriter wi) BittrBufferUnlimited r where
  toHoley = immediate . appendUnlimited

instance (ToBitBufferBuilder wi,  Monoid wi) => ToHoley (BittrWriter wi) BittrBuffer r where
  toHoley = immediate . appendBittrBuffer
