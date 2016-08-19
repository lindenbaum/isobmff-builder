{-# LANGUAGE UndecidableInstances #-}
module Data.Type.BitRecords.Builder.LazyByteStringBuilder where

import           Data.Type.BitRecords.Builder.BitBuffer
import           Data.Type.BitRecords.Builder.Holey
import           Data.Type.BitRecords.Core
import           Data.Word
import           Data.Bits
import           Data.Proxy
import           GHC.TypeLits
import           Data.Monoid
import           Control.Category
import           Data.Tagged
import           Prelude hiding ( (.), id )
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy as B
import           Text.Printf

----------------
----------------
----------------

newtype BittrWriter = BittrWriter { unBittrWriter :: Dual (Endo BittrWriterState) }
  deriving Monoid

bittrWriter :: (BittrWriterState -> BittrWriterState) -> BittrWriter
bittrWriter = BittrWriter . Dual . Endo

runBittrWriter :: BittrWriter -> Builder
runBittrWriter !w = getBittrWriteStateBuilder $
    flushBittrWriter $ appBittrWriter w initialBittrWriterState

appBittrWriter :: BittrWriter -> BittrWriterState -> BittrWriterState
appBittrWriter !w = appEndo (getDual (unBittrWriter w))

data BittrWriterState where
    BittrWriterState :: !Builder -> !BitOutBuffer -> BittrWriterState

getBittrWriteStateBuilder :: BittrWriterState -> Builder
getBittrWriteStateBuilder (BittrWriterState !builder _) = builder

initialBittrWriterState :: BittrWriterState
initialBittrWriterState = BittrWriterState mempty emptyBitOutBuffer

-- | Write the partial buffer contents using  any number of 'word8'
--   The unwritten parts of the bittr buffer are at the top.
--   If the
--
-- >     63  ...  (63-off-1)(63-off)  ...  0
-- >     ^^^^^^^^^^^^^^^^^^^
-- > Relevant bits start to the top!
--
flushBittrWriter :: BittrWriterState -> BittrWriterState
flushBittrWriter (BittrWriterState !bldr !buff) =
    BittrWriterState (writeRestBytes bldr 0) emptyBitOutBuffer
  where
    !off = bitOutBufferLength buff
    !part = unBitBuffer (bitOutBufferContent buff)
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
                     word8 (fromIntegral ((part `unsafeShiftR`
                                               (bitBufferSize -
                                                    flushOffset')) .&.
                                              0xFF))
             in
                 writeRestBytes bldr'' flushOffset'



printBuilder :: Builder -> String
printBuilder b =
      ("<< " ++)
   $  (++" >>")
   $  unwords
   $  printf "%0.2x"
  <$>  B.unpack (toLazyByteString b)

---

-- | Write all the bits, in chunks, filling and writing the 'BittrBuffer'
-- in the 'BittrWriterState' as often as necessary.
appendUnlimited :: BittrBufferUnlimited -> BittrWriter
appendUnlimited x' = bittrWriter $
    \(BittrWriterState !builder !buff) -> go x' builder buff
  where
    go !x !builder !buff
        | isBittrBufferUnlimitedEmpty x =
              BittrWriterState builder buff
        | otherwise = let (!rest, !buff') = bufferBitsInteger x buff
                      in
                          if bitOutBufferSpaceLeft buff' > 0
                          then BittrWriterState builder buff'
                          else let !nextBuilder = builder <>
                                       word64BE (unBitBuffer (bitOutBufferContent buff'))
                               in
                                   go rest nextBuilder emptyBitOutBuffer


-- | Write all the bits, in chunks, filling and writing the 'BittrBuffer'
-- in the 'BittrWriterState' as often as necessary.
appendBittrBuffer :: BittrBuffer -> BittrWriter
appendBittrBuffer x' = bittrWriter $
    \(BittrWriterState !builder !buff) -> go x' builder buff
  where
    go !x !builder !buff
        | bittrBufferLength x == 0 =
              BittrWriterState builder buff
        | otherwise = let (!rest, !buff') = bufferBits x buff
                      in
                          if bitOutBufferSpaceLeft buff' > 0
                          then BittrWriterState builder buff'
                          else let !nextBuilder = builder <>
                                       word64BE (unBitBuffer (bitOutBufferContent buff'))
                               in
                                   go rest nextBuilder emptyBitOutBuffer

runBittrWriterHoley :: Holey BittrWriter Builder a -> a
runBittrWriterHoley (HM !x) = x runBittrWriter

instance ToHoley BittrWriter BittrBuffer r where
  toHoley = immediate . appendBittrBuffer

instance ToHoley BittrWriter BittrBufferUnlimited r where
  toHoley = immediate . appendUnlimited

instance KnownBitBufferSize (GetRecordSize f) =>
         ToHoley BittrWriter (Proxy (l :=> f)) r where
    type ToM BittrWriter (Proxy (l :=> f)) r = Tagged l Word64 -> r
    toHoley _ = indirect (appendBittrBuffer . bittrBufferProxyLength fieldLen . untag)
      where
        !fieldLen = Proxy :: Proxy (GetRecordSize f)

instance (KnownNat v, KnownBitBufferSize (GetRecordSize f)) =>
         ToHoley BittrWriter (Proxy (f := v)) r where
    toHoley _ = immediate (appendBittrBuffer fieldBittrBuffer)
      where
        !fieldBittrBuffer = bittrBufferProxyLength fieldLen (fromIntegral fieldVal)
          where
            !fieldLen = Proxy :: Proxy (GetRecordSize f)
            !fieldVal = natVal (Proxy :: Proxy v)

instance (KnownBitBufferSize n) =>
         ToHoley BittrWriter (Proxy (Field n)) r where
    toHoley _ = immediate (appendBittrBuffer (bittrBufferProxyLength (Proxy :: Proxy n)
                                                                     0))
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
instance ( KnownNat (GetRecordSize (f0 :>: f1))
         , ToHoley BittrWriter (Proxy f0) (ToM BittrWriter (Proxy f1) r)
         , ToHoley BittrWriter (Proxy f1) r) =>
         ToHoley BittrWriter (Proxy (f0 :>: f1)) r where
    type ToM BittrWriter (Proxy (f0 :>: f1)) r =
        ToM BittrWriter (Proxy f0) (ToM BittrWriter (Proxy f1) r)
    toHoley _ = fmt0 . fmt1
      where
        !fmt0 = toHoley pf0
        !fmt1 = toHoley pf1
        pf0 = Proxy :: Proxy f0
        pf1 = Proxy :: Proxy f1
