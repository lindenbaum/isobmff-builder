{-# LANGUAGE UndecidableInstances #-}
module Data.Type.BitRecords.Builder.LazyByteStringBuilder where

import           Data.Traversable
import           Data.Type.BitRecords.Builder.BitBuffer
import           Data.Type.BitRecords.Builder.Holey
import           Data.Type.BitRecords.Core
import           Data.Word
import           Data.Int
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

instance forall f r l
         . (ToHoley BitStringBuilder (Proxy f) r, ToM BitStringBuilder (Proxy f) r ~ (FieldRep f -> r))
         => ToHoley BitStringBuilder (Proxy (l :=> f)) r where
    type ToM BitStringBuilder (Proxy (l :=> f)) r = Tagged l (FieldRep f) -> r
    toHoley _ = let (HM !fm) = toHoley (Proxy :: Proxy f)
                in HM (\ !k !vt -> fm k (untag vt))

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

instance forall n r . (KnownChunkSize n, FieldRep (Field n) ~ Word64) =>
         ToHoley BitStringBuilder (Proxy (Field n)) r where
    type ToM BitStringBuilder (Proxy (Field n)) r = FieldRep (Field n) -> r
    toHoley _ = indirect (appendBitString . bitStringProxyLength (Proxy :: Proxy n))

instance ToHoley BitStringBuilder (Proxy Word64) r where
    type ToM BitStringBuilder (Proxy Word64) r = FieldRep Word64 -> r
    toHoley _ = indirect (appendBitString . flip bitString 64)

instance ToHoley BitStringBuilder (Proxy Word32) r where
    type ToM BitStringBuilder (Proxy Word32) r = FieldRep Word32 -> r
    toHoley _ = indirect (appendBitString . flip bitString 32 . fromIntegral)

instance ToHoley BitStringBuilder (Proxy Word16) r where
    type ToM BitStringBuilder (Proxy Word16) r = FieldRep Word16 -> r
    toHoley _ = indirect (appendBitString . flip bitString 16 . fromIntegral)

instance ToHoley BitStringBuilder (Proxy Word8) r where
    type ToM BitStringBuilder (Proxy Word8) r = FieldRep Word8 -> r
    toHoley _ = indirect (appendBitString . flip bitString 8 . fromIntegral)

instance ToHoley BitStringBuilder (Proxy Int64) r where
    type ToM BitStringBuilder (Proxy Int64) r = FieldRep Int64 -> r
    toHoley _ = indirect (appendBitString . flip bitString 64 . fromIntegral)

instance ToHoley BitStringBuilder (Proxy Int32) r where
    type ToM BitStringBuilder (Proxy Int32) r = FieldRep Int32 -> r
    toHoley _ =
      indirect (appendBitString . flip bitString 32 . fromIntegral . (fromIntegral :: FieldRep Int32 -> Word32))

instance ToHoley BitStringBuilder (Proxy Int16) r where
    type ToM BitStringBuilder (Proxy Int16) r = FieldRep Int16 -> r
    toHoley _ =
      indirect (appendBitString . flip bitString 16 . fromIntegral . (fromIntegral :: FieldRep Int16 -> Word16))

instance ToHoley BitStringBuilder (Proxy Int8) r where
    type ToM BitStringBuilder (Proxy Int8) r = FieldRep Int8 -> r
    toHoley _ =
      indirect (appendBitString . flip bitString 8 . fromIntegral . (fromIntegral :: FieldRep Int8 -> Word8))

instance ToHoley BitStringBuilder (Proxy Bool) r where
    type ToM BitStringBuilder (Proxy Bool) r = FieldRep Bool -> r
    toHoley _ =
      indirect (appendBitString . flip bitString 1 . fromIntegral . fromEnum)

instance ToHoley BitStringBuilder (Proxy 'False) r where
    type ToM BitStringBuilder (Proxy 'False) r = r
    toHoley _ = immediate (appendBitString $ bitString 0 1)

instance ToHoley BitStringBuilder (Proxy 'True) r where
    type ToM BitStringBuilder (Proxy 'True) r = r
    toHoley _ = immediate (appendBitString $ bitString 1 1)

instance ( KnownNat v
         , Num (FieldRep f)
         , ToHoley BitStringBuilder (Proxy f) r
         , ToM BitStringBuilder (Proxy f) r ~ (FieldRep f -> r)
         ) => ToHoley BitStringBuilder (Proxy (f := (v :: Nat))) r where

    toHoley _ = let (HM !fm) = toHoley (Proxy :: Proxy f)
                    !fieldVal = natVal (Proxy :: Proxy v)
                in HM (\ !k -> fm k (fromIntegral fieldVal))

instance ( KnownNat v
         , Num (FieldRep f)
         , ToHoley BitStringBuilder (Proxy f) r
         , ToM BitStringBuilder (Proxy f) r ~ (FieldRep f -> r)
         ) => ToHoley BitStringBuilder (Proxy (f := Negative v)) r where

    toHoley _ = let (HM !fm) = toHoley (Proxy :: Proxy f)
                    !fieldVal = natVal (Proxy :: Proxy v)
                in HM (\ !k -> fm k (-1 * fromIntegral fieldVal))
