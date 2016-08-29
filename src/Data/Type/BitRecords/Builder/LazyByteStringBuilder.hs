{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints  #-}

module Data.Type.BitRecords.Builder.LazyByteStringBuilder where

import Data.Coerce
import Data.Type.BitRecords.Builder.BitBuffer
import Data.Type.BitRecords.Builder.Holey
import Data.Type.BitRecords.Core
import Data.Word
import Data.Int
import Data.Bits
import Data.Kind (Constraint)
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

-- * 'BitString' construction from 'BitRecord's

class BitStringBuilderHoley a r where
  type ToBitStringBuilder a r
  type ToBitStringBuilder a r = r
  bitStringBuilderHoley :: a -> Holey BitStringBuilder r (ToBitStringBuilder a r)

instance BitStringBuilderHoley BitString r where
  bitStringBuilderHoley = immediate . appendBitString

-- ** 'BitRecordField' instances

type family MaybeKnownLabel (l :: Maybe Symbol) :: Constraint where
  MaybeKnownLabel 'Nothing = ()
  MaybeKnownLabel ('Just l) = (KnownSymbol l)

type family MaybeLabelledParameter (l :: Maybe Symbol) x where
  MaybeLabelledParameter 'Nothing x = x
  MaybeLabelledParameter ('Just l) x = Tagged l x

type family UnsignedDemoteRep i where
  UnsignedDemoteRep Int8  = Word8
  UnsignedDemoteRep Int16 = Word16
  UnsignedDemoteRep Int32 = Word32
  UnsignedDemoteRep Int64 = Word64

-- *** Fields carrying runtime values

instance (Enum (MaybeLabelledParameter l Bool), MaybeKnownLabel l)
  => BitStringBuilderHoley (Proxy ('BitRecordField l Bool 1 ('Nothing :: Maybe Bool))) a where
  type ToBitStringBuilder (Proxy ('BitRecordField l Bool 1 ('Nothing :: Maybe Bool))) a =
    MaybeLabelledParameter l Bool -> a
  bitStringBuilderHoley _ =
    indirect
      (appendBitString
         . bitString 1
         . fromIntegral
         . fromEnum)

instance forall l r s a . (Integral (MaybeLabelledParameter l r), MaybeKnownLabel l, KnownChunkSize s)
  => BitStringBuilderHoley (Proxy ('BitRecordField l r s ('Nothing :: Maybe Nat))) a where
  type ToBitStringBuilder (Proxy ('BitRecordField l r s ('Nothing :: Maybe Nat))) a
    = MaybeLabelledParameter l r -> a
  bitStringBuilderHoley _ =
    indirect
      (appendBitString
         . bitStringProxyLength (Proxy @s)
         . fromIntegral)

instance forall l r s a . ( Integral (UnsignedDemoteRep r)
                     , Integral (MaybeLabelledParameter l r)
                     , Integral r
                     , Coercible (MaybeLabelledParameter l r) r
                     , MaybeKnownLabel l
                     , KnownChunkSize s)
  => BitStringBuilderHoley (Proxy ('BitRecordField l r s ('Nothing :: Maybe SignedNat))) a where
  type ToBitStringBuilder (Proxy ('BitRecordField l r s ('Nothing :: Maybe SignedNat))) a =
    MaybeLabelledParameter l r -> a
  bitStringBuilderHoley _ =
    indirect
      (appendBitString
         . bitStringProxyLength (Proxy @s)
         . fromIntegral
         . (fromIntegral @r @(UnsignedDemoteRep r))
         . coerce)

-- *** Static Fields

instance
  BitStringBuilderHoley (Proxy ('BitRecordField l () 1 ('Just 'False))) a where
  bitStringBuilderHoley _ = immediate (appendBitString (bitString 1 0))

instance
  BitStringBuilderHoley (Proxy ('BitRecordField l () 1 ('Just 'True))) a where
  bitStringBuilderHoley _ = immediate (appendBitString (bitString 1 1))

instance forall r s v a . (KnownChunkSize s, KnownNat v)
  => BitStringBuilderHoley (Proxy ('BitRecordField 'Nothing r s ('Just (v :: Nat)))) a where
  bitStringBuilderHoley _ =
    immediate
    $ appendBitString
    $ bitStringProxyLength (Proxy @s)
    $ fromIntegral
    $ natVal
    $ Proxy @v

instance forall r l n s a . (KnownNat n, KnownChunkSize s, Integral (UnsignedDemoteRep r))
  => BitStringBuilderHoley (Proxy ('BitRecordField l r s ('Just ('NegativeNat n)))) a where
  bitStringBuilderHoley _ =
    immediate
    $ appendBitString
    $ bitStringProxyLength (Proxy @s)
    $ fromIntegral
    $ fromIntegral @Integer @(UnsignedDemoteRep r)
    $ (* (-1))
    $ natVal
    $ Proxy @n

instance forall r l n s a . (KnownNat n, KnownChunkSize s)
  => BitStringBuilderHoley (Proxy ('BitRecordField l r s ('Just ('PositiveNat n)))) a where
  bitStringBuilderHoley _ =
    immediate
    $ appendBitString
    $ bitStringProxyLength (Proxy @s)
    $ fromIntegral
    $ natVal
    $ Proxy @n

-- ** 'BitRecord' instances

instance forall f a . BitStringBuilderHoley (Proxy f) a => BitStringBuilderHoley (Proxy ('BitRecordMember f)) a where
  type ToBitStringBuilder (Proxy ('BitRecordMember f)) a = ToBitStringBuilder (Proxy f) a
  bitStringBuilderHoley _ = bitStringBuilderHoley (Proxy @f)

instance forall l r a .
  (BitStringBuilderHoley (Proxy l) (ToBitStringBuilder (Proxy r) a)
  , BitStringBuilderHoley (Proxy r) a)
   => BitStringBuilderHoley (Proxy ('AppendedBitRecords l r)) a where
  type ToBitStringBuilder (Proxy ('AppendedBitRecords l r)) a =
    ToBitStringBuilder (Proxy l) (ToBitStringBuilder (Proxy r) a)
  bitStringBuilderHoley _ = bitStringBuilderHoley (Proxy @l) . bitStringBuilderHoley (Proxy @r)

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
