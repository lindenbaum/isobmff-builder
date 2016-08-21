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

newtype BitStringBuilder =
      BitStringBuilder { unBitStringBuilder :: Dual (Endo BitStringBuilderState)
                       }
    deriving Monoid

bitStringBuilder :: (BitStringBuilderState -> BitStringBuilderState)
                 -> BitStringBuilder
bitStringBuilder = BitStringBuilder . Dual . Endo

runBitStringBuilder :: BitStringBuilder -> Builder
runBitStringBuilder !w =
    getBitStringBuilderStateBuilder $
        flushBitStringBuilder $
            appBitStringBuilder w initialBitStringBuilderState

appBitStringBuilder :: BitStringBuilder
                    -> BitStringBuilderState
                    -> BitStringBuilderState
appBitStringBuilder !w =
    appEndo (getDual (unBitStringBuilder w))

data BitStringBuilderState where
        BitStringBuilderState ::
          !Builder -> !BitStringBuilderChunk -> BitStringBuilderState

getBitStringBuilderStateBuilder :: BitStringBuilderState -> Builder
getBitStringBuilderStateBuilder (BitStringBuilderState !builder _) =
    builder

initialBitStringBuilderState :: BitStringBuilderState
initialBitStringBuilderState =
    BitStringBuilderState mempty emptyBitStringBuilderChunk

-- | Write the partial buffer contents using any number of 'word8' The unwritten
--   parts of the bittr buffer are at the top.  If the
--
-- >     63  ...  (63-off-1)(63-off)  ...  0
-- >     ^^^^^^^^^^^^^^^^^^^
-- > Relevant bits start to the top!
--
flushBitStringBuilder :: BitStringBuilderState -> BitStringBuilderState
flushBitStringBuilder (BitStringBuilderState !bldr !buff) =
    BitStringBuilderState (writeRestBytes bldr 0) emptyBitStringBuilderChunk
  where
    !off = bitStringBuilderChunkLength buff
    !part = bitStringBuilderChunkContent buff
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
                                               (bitStringMaxLength -
                                                    flushOffset')) .&.
                                              0xFF))
             in
                 writeRestBytes bldr'' flushOffset'



printBuilder :: Builder -> String
printBuilder b = ("<< " ++)
    $ (++ " >>")
        $ unwords
            $ printf "%0.2x" <$> B.unpack (toLazyByteString b)

---


-- | Write all the bits, in chunks, filling and writing the 'BitString'
-- in the 'BitStringBuilderState' as often as necessary.
appendBitString :: BitString -> BitStringBuilder
appendBitString x' = bitStringBuilder $
    \(BitStringBuilderState !builder !buff) -> go x' builder buff
  where
    go !x !builder !buff
        | bitStringLength x == 0 =
              BitStringBuilderState builder buff
        | otherwise = let (!rest, !buff') = bufferBits x buff
                      in
                          if bitStringBuilderChunkSpaceLeft buff' > 0
                          then BitStringBuilderState builder buff'
                          else let !nextBuilder = builder <>
                                       word64BE (bitStringBuilderChunkContent buff')
                               in
                                   go rest nextBuilder emptyBitStringBuilderChunk

runBitStringBuilderHoley :: Holey BitStringBuilder Builder a -> a
runBitStringBuilderHoley (HM !x) =
    x runBitStringBuilder

instance ToHoley BitStringBuilder BitString r where
    toHoley = immediate . appendBitString

instance KnownChunkSize (GetRecordSize f) =>
         ToHoley BitStringBuilder (Proxy (l :=> f)) r where
    type ToM BitStringBuilder (Proxy (l :=> f)) r = Tagged l Word64 -> r
    toHoley _ = indirect (appendBitString .
                              bitStringProxyLength fieldLen . untag)
      where
        !fieldLen = Proxy :: Proxy (GetRecordSize f)

instance (KnownNat v, KnownChunkSize (GetRecordSize f)) =>
         ToHoley BitStringBuilder (Proxy (f := v)) r where
    toHoley _ = immediate (appendBitString fieldBitString)
      where
        !fieldBitString = bitStringProxyLength fieldLen
                                                   (fromIntegral fieldVal)
          where
            !fieldLen = Proxy :: Proxy (GetRecordSize f)
            !fieldVal = natVal (Proxy :: Proxy v)

instance (KnownChunkSize n) =>
         ToHoley BitStringBuilder (Proxy (Field n)) r where
    toHoley _ = immediate (appendBitString (bitStringProxyLength (Proxy :: Proxy n)
                                                                     0))
instance ( KnownNat (GetRecordSize (f0 :>: f1))
         , ToHoley BitStringBuilder (Proxy f0) (ToM BitStringBuilder (Proxy f1) r)
         , ToHoley BitStringBuilder (Proxy f1) r) =>
         ToHoley BitStringBuilder (Proxy (f0 :>: f1)) r where
    type ToM BitStringBuilder (Proxy (f0 :>: f1)) r =
        ToM BitStringBuilder (Proxy f0) (ToM BitStringBuilder (Proxy f1) r)
    toHoley _ = fmt0 . fmt1
      where
        !fmt0 = toHoley pf0
        !fmt1 = toHoley pf1
        pf0 = Proxy :: Proxy f0
        pf1 = Proxy :: Proxy f1
