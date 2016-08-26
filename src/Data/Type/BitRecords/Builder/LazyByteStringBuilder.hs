{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints  #-}

module Data.Type.BitRecords.Builder.LazyByteStringBuilder where

import Data.Type.BitRecords.Builder.BitBuffer
import Data.Type.BitRecords.Builder.Holey
import Data.Type.BitRecords.Core
import Data.Word
import Data.Int
import Data.Bits
import Data.Proxy
import GHC.TypeLits
import Data.Monoid
import Control.Category
import Data.Tagged
import Prelude hiding ((.), id)
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as SB
import Text.Printf

newtype BitStringBuilder =
  BitStringBuilder {unBitStringBuilder :: Dual (Endo BitStringBuilderState)}
  deriving (Monoid)

runBitStringBuilder
  :: BitStringBuilder -> Builder
runBitStringBuilder !w =
  getBitStringBuilderStateBuilder $
  flushBitStringBuilder $ appBitStringBuilder w initialBitStringBuilderState

bitStringBuilder :: (BitStringBuilderState -> BitStringBuilderState)
                 -> BitStringBuilder
bitStringBuilder = BitStringBuilder . Dual . Endo

appBitStringBuilder :: BitStringBuilder
                    -> BitStringBuilderState
                    -> BitStringBuilderState
appBitStringBuilder !w = appEndo (getDual (unBitStringBuilder w))

data BitStringBuilderState where
        BitStringBuilderState ::
          !Builder -> !BitStringBuilderChunk -> BitStringBuilderState

getBitStringBuilderStateBuilder
  :: BitStringBuilderState -> Builder
getBitStringBuilderStateBuilder (BitStringBuilderState !builder _) = builder

initialBitStringBuilderState
  :: BitStringBuilderState
initialBitStringBuilderState =
  BitStringBuilderState mempty emptyBitStringBuilderChunk

-- | Write the partial buffer contents using any number of 'word8' The unwritten
--   parts of the bittr buffer are at the top.  If the
--
-- >     63  ...  (63-off-1)(63-off)  ...  0
-- >     ^^^^^^^^^^^^^^^^^^^
-- > Relevant bits start to the top!
--
flushBitStringBuilder
  :: BitStringBuilderState -> BitStringBuilderState
flushBitStringBuilder (BitStringBuilderState !bldr !buff) =
  BitStringBuilderState (writeRestBytes bldr 0)
                        emptyBitStringBuilderChunk
  where !off = bitStringBuilderChunkLength buff
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
                      !bldr'' =
                        bldr' <>
                        word8 (fromIntegral
                                 ((part `unsafeShiftR`
                                   (bitStringMaxLength - flushOffset')) .&.
                                  0xFF))
                  in writeRestBytes bldr'' flushOffset'

-- | Write all the bits, in chunks, filling and writing the 'BitString'
-- in the 'BitStringBuilderState' as often as necessary.
appendBitString :: BitString -> BitStringBuilder
appendBitString !x' =
  bitStringBuilder $
  \(BitStringBuilderState !builder !buff) -> go x' builder buff
  where go !x !builder !buff
          | bitStringLength x == 0 = BitStringBuilderState builder buff
          | otherwise =
            let (!rest,!buff') = bufferBits x buff
            in if bitStringBuilderChunkSpaceLeft buff' > 0
                  then BitStringBuilderState builder buff'
                  else let !nextBuilder =
                             builder <>
                             word64BE (bitStringBuilderChunkContent buff')
                       in go rest nextBuilder emptyBitStringBuilderChunk

-- | Write all the b*y*tes, into the 'BitStringBuilderState' this allows general
-- purposes non-byte aligned builders.
appendStrictByteString :: SB.ByteString -> BitStringBuilder
appendStrictByteString !sb =
  foldMap (appendBitString . bitString 8 . fromIntegral) (SB.unpack sb)

runBitStringBuilderHoley
  :: Holey BitStringBuilder Builder a -> a
runBitStringBuilderHoley (HM !x) = x runBitStringBuilder

class BitStringBuilderHoley a r where
  type ToBitStringBuilder a r
  type ToBitStringBuilder a r = r
  bitStringBuilderHoley :: a -> Holey BitStringBuilder r (ToBitStringBuilder a r)

instance BitStringBuilderHoley BitString r where
  bitStringBuilderHoley = immediate . appendBitString

instance forall f r l . (BitStringBuilderHoley (Proxy f) r
              ,ToBitStringBuilder (Proxy f) r ~ (FieldRep f -> r)) =>
         BitStringBuilderHoley (Proxy (l :=> f)) r where
  type ToBitStringBuilder (Proxy (l :=> f)) r = Tagged l (FieldRep f) -> r
  bitStringBuilderHoley _ =
    let (HM !fm) = bitStringBuilderHoley (Proxy :: Proxy f)
    in HM (\ !k !vt -> fm k (untag vt))

instance (KnownNat (GetRecordSize (f0 :>: f1))
         ,BitStringBuilderHoley (Proxy f0) (ToBitStringBuilder (Proxy f1) r)
         ,BitStringBuilderHoley (Proxy f1) r) =>
         BitStringBuilderHoley (Proxy (f0 :>: f1)) r where
  type ToBitStringBuilder (Proxy (f0 :>: f1)) r = ToBitStringBuilder (Proxy f0) (ToBitStringBuilder (Proxy f1) r)
  bitStringBuilderHoley _ = fmt0 . fmt1
    where !fmt0 = bitStringBuilderHoley pf0
          !fmt1 = bitStringBuilderHoley pf1
          pf0 = Proxy :: Proxy f0
          pf1 = Proxy :: Proxy f1

instance forall n r . (KnownChunkSize n
            ,FieldRep (Field n) ~ Word64) =>
         BitStringBuilderHoley (Proxy (Field n)) r where
  type ToBitStringBuilder (Proxy (Field n)) r = FieldRep (Field n) -> r
  bitStringBuilderHoley _ =
    indirect (appendBitString . bitStringProxyLength (Proxy :: Proxy n))

instance BitStringBuilderHoley (Proxy Word64) r where
  type ToBitStringBuilder (Proxy Word64) r = FieldRep Word64 -> r
  bitStringBuilderHoley _ = indirect (appendBitString . bitString 64)

instance BitStringBuilderHoley (Proxy Word32) r where
  type ToBitStringBuilder (Proxy Word32) r = FieldRep Word32 -> r
  bitStringBuilderHoley _ = indirect (appendBitString . bitString 32 . fromIntegral)

instance BitStringBuilderHoley (Proxy Word16) r where
  type ToBitStringBuilder (Proxy Word16) r = FieldRep Word16 -> r
  bitStringBuilderHoley _ = indirect (appendBitString . bitString 16 . fromIntegral)

instance BitStringBuilderHoley (Proxy Word8) r where
  type ToBitStringBuilder (Proxy Word8) r = FieldRep Word8 -> r
  bitStringBuilderHoley _ = indirect (appendBitString . bitString 8 . fromIntegral)

instance BitStringBuilderHoley (Proxy Int64) r where
  type ToBitStringBuilder (Proxy Int64) r = FieldRep Int64 -> r
  bitStringBuilderHoley _ = indirect (appendBitString . bitString 64 . fromIntegral)

instance BitStringBuilderHoley (Proxy Int32) r where
  type ToBitStringBuilder (Proxy Int32) r = FieldRep Int32 -> r
  bitStringBuilderHoley _ =
    indirect (appendBitString .
              bitString 32 .
              fromIntegral . (fromIntegral :: FieldRep Int32 -> Word32))

instance BitStringBuilderHoley (Proxy Int16) r where
  type ToBitStringBuilder (Proxy Int16) r = FieldRep Int16 -> r
  bitStringBuilderHoley _ =
    indirect (appendBitString .
              bitString 16 .
              fromIntegral . (fromIntegral :: FieldRep Int16 -> Word16))

instance BitStringBuilderHoley (Proxy Int8) r where
  type ToBitStringBuilder (Proxy Int8) r = FieldRep Int8 -> r
  bitStringBuilderHoley _ =
    indirect (appendBitString .
              bitString 8 .
              fromIntegral . (fromIntegral :: FieldRep Int8 -> Word8))

instance BitStringBuilderHoley (Proxy Bool) r where
  type ToBitStringBuilder (Proxy Bool) r = FieldRep Bool -> r
  bitStringBuilderHoley _ =
    indirect (appendBitString . bitString 1 . fromIntegral . fromEnum)

instance BitStringBuilderHoley (Proxy 'False) r where
  type ToBitStringBuilder (Proxy 'False) r = r
  bitStringBuilderHoley _ = immediate (appendBitString $ bitString 1 0)

instance BitStringBuilderHoley (Proxy 'True) r where
  type ToBitStringBuilder (Proxy 'True) r = r
  bitStringBuilderHoley _ = immediate (appendBitString $ bitString 1 1)

instance BitStringBuilderHoley (Proxy (FlagJust ('Just t))) r where
  bitStringBuilderHoley _ = immediate (appendBitString $ bitString 1 1)

instance BitStringBuilderHoley (Proxy (FlagJust 'Nothing)) r where
  bitStringBuilderHoley _ = immediate (appendBitString $ bitString 1 0)

instance BitStringBuilderHoley (Proxy (FlagNothing ('Just t))) r where
  bitStringBuilderHoley _ = immediate (appendBitString $ bitString 1 0)

instance BitStringBuilderHoley (Proxy (FlagNothing 'Nothing)) r where
  bitStringBuilderHoley _ = immediate (appendBitString $ bitString 1 1)

instance BitStringBuilderHoley (Proxy 'Nothing) r where
  bitStringBuilderHoley _ = id

instance forall x r . BitStringBuilderHoley (Proxy x) r
  => BitStringBuilderHoley (Proxy ('Just x)) r where
  type ToBitStringBuilder (Proxy ('Just x)) r = ToBitStringBuilder (Proxy x) r
  bitStringBuilderHoley _ = bitStringBuilderHoley (Proxy :: Proxy x)

instance BitStringBuilderHoley (Proxy '[]) r where
  bitStringBuilderHoley _ = id

instance forall x xs r .
  ( BitStringBuilderHoley (Proxy x) (ToBitStringBuilder (Proxy xs) r)
  , BitStringBuilderHoley (Proxy xs) r )
  => BitStringBuilderHoley (Proxy (x ': xs)) r where
  type ToBitStringBuilder (Proxy (x ': xs)) r =
    ToBitStringBuilder (Proxy x) (ToBitStringBuilder (Proxy xs) r)
  bitStringBuilderHoley _ =
    bitStringBuilderHoley (Proxy :: Proxy x) . bitStringBuilderHoley (Proxy :: Proxy xs)

instance forall x r fieldSize .
  ( BitStringBuilderHoley (Proxy x) r
  , KnownNat (SizeFieldValue x)
  , KnownNat (GetSizeFieldFieldSize fieldSize))
  => BitStringBuilderHoley (Proxy (Sized fieldSize x)) r where
  type ToBitStringBuilder (Proxy (Sized fieldSize x)) r =
    ToBitStringBuilder (Proxy x) r
  bitStringBuilderHoley _ = writeSizeField . bitStringBuilderHoley (Proxy :: Proxy x)
    where
      writeSizeField
        | sizeFieldSize == 0 = id
        | elementCount >= 2 ^ sizeFieldSize =
            error $ printf "invalid elementCount: %d for field size: %d"
                           elementCount sizeFieldSize
        | otherwise =
            immediate $
              appendBitString $ bitString sizeFieldSize elementCount
        where
          elementCount  = fromIntegral $ natVal (Proxy :: Proxy (SizeFieldValue x))
          sizeFieldSize = fromIntegral $ natVal (Proxy :: Proxy (GetSizeFieldFieldSize fieldSize))

instance forall x r n .
  ( BitStringBuilderHoley (Proxy (ReplicateRecord n x)) r )
  => BitStringBuilderHoley (Proxy (RecArray x n)) r where
  type ToBitStringBuilder (Proxy (RecArray x n)) r =
    ToBitStringBuilder (Proxy (ReplicateRecord n x)) r
  bitStringBuilderHoley _ =
    bitStringBuilderHoley (Proxy :: Proxy (ReplicateRecord n x))

instance (KnownNat v
         ,Num (FieldRep f)
         ,BitStringBuilderHoley (Proxy f) r
         ,ToBitStringBuilder (Proxy f) r ~ (FieldRep f -> r)) =>
         BitStringBuilderHoley (Proxy (f := (v :: Nat))) r where
  bitStringBuilderHoley _ =
    let (HM !fm) = bitStringBuilderHoley (Proxy :: Proxy f)
        !fieldVal = natVal (Proxy :: Proxy v)
    in HM (\ !k -> fm k (fromIntegral fieldVal))

instance (KnownNat v
         ,Num (FieldRep f)
         ,BitStringBuilderHoley (Proxy f) r
         ,ToBitStringBuilder (Proxy f) r ~ (FieldRep f -> r)) =>
         BitStringBuilderHoley (Proxy (f := Negative v)) r where
  bitStringBuilderHoley _ =
    let (HM !fm) = bitStringBuilderHoley (Proxy :: Proxy f)
        !fieldVal = natVal (Proxy :: Proxy v)
    in HM (\ !k -> fm k (-1 * fromIntegral fieldVal))

-- ** Tracing/Debug Printing
printBuilder :: Builder -> String
printBuilder b =
  ("<< " ++) $
  (++ " >>") $ unwords $ printf "%0.2x" <$> B.unpack (toLazyByteString b)

bitStringPrinter
  :: BitStringBuilderHoley a String
  => a -> ToBitStringBuilder a String
bitStringPrinter =
  runHoley . hoistM (printBuilder . runBitStringBuilder) . bitStringBuilderHoley
