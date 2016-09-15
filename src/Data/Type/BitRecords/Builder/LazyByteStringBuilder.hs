{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints  #-}
module Data.Type.BitRecords.Builder.LazyByteStringBuilder where

import Data.Type.BitRecords.Builder.BitBuffer
import Data.Type.BitRecords.Builder.Holey
import Data.Type.BitRecords.Core
import Data.Word
import Data.Int
import Data.Bits
import Data.Kind.Extra
import Data.Proxy
import GHC.TypeLits
import Data.Monoid
import Control.Category
import Prelude hiding ((.), id)
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as SB
import Text.Printf

-- | A wrapper around a builder derived from a 'BitStringBuilderState'
data BuilderBox where
  MkBuilderBox :: !Word64 -> !Builder -> BuilderBox

-- | Create a 'Builder' from a 'BitRecord' and store it in a 'BuilderBox';
-- return a 'Holey' monoid that does that on 'runHoley'
bitBuilderBox ::
  forall (record :: BitRecord) r .
  BitStringBuilderHoley (Proxy record) r
  =>  Proxy record
  -> Holey BuilderBox r (ToBitStringBuilder (Proxy record) r)
bitBuilderBox !p =
  let fromBitStringBuilder !h =
        let (BitStringBuilderState !builder _ !wsize) =
              flushBitStringBuilder
              $ appBitStringBuilder h initialBitStringBuilderState
            !out = MkBuilderBox wsize builder
        in out
  in hoistM fromBitStringBuilder (bitStringBuilderHoley p)

-- | Like 'bitBuilderBox', accept as an additional parameter a wrapper function
-- to wrap the final result (the 'BuilderBox') and 'runHoley' the whole
-- machiner.
wrapBitBuilderBox ::
  forall (record :: BitRecord) wrapped .
    BitStringBuilderHoley (Proxy record) wrapped
  => (BuilderBox -> wrapped)
  -> Proxy record
  -> ToBitStringBuilder (Proxy record) wrapped
wrapBitBuilderBox !f !p = runHoley (hoistM f (bitBuilderBox p))

-- * Low-level interface to building 'BitRecord's and other things

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
          !Builder -> !BitStringBuilderChunk -> !Word64 -> BitStringBuilderState

getBitStringBuilderStateBuilder
  :: BitStringBuilderState -> Builder
getBitStringBuilderStateBuilder (BitStringBuilderState !builder _ _) = builder

initialBitStringBuilderState
  :: BitStringBuilderState
initialBitStringBuilderState =
  BitStringBuilderState mempty emptyBitStringBuilderChunk 0

-- | Write the partial buffer contents using any number of 'word8' The unwritten
--   parts of the bittr buffer are at the top.  If the
--
-- >     63  ...  (63-off-1)(63-off)  ...  0
-- >     ^^^^^^^^^^^^^^^^^^^
-- > Relevant bits start to the top!
--
flushBitStringBuilder
  :: BitStringBuilderState -> BitStringBuilderState
flushBitStringBuilder (BitStringBuilderState !bldr !buff !totalSize) =
  BitStringBuilderState (writeRestBytes bldr 0)
                        emptyBitStringBuilderChunk
                        totalSize'
  where !off = bitStringBuilderChunkLength buff
        !off_ = (fromIntegral off :: Word64)
        !totalSize' = totalSize + signum (off_ `rem` 8) + (off_ `div` 8)
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
  \(BitStringBuilderState !builder !buff !totalSizeIn) -> go x' builder buff totalSizeIn
  where go !x !builder !buff !totalSize
          | bitStringLength x == 0 = BitStringBuilderState builder buff totalSize
          | otherwise =
            let (!rest, !buff') = bufferBits x buff
            in if bitStringBuilderChunkSpaceLeft buff' > 0
                  then BitStringBuilderState builder buff' totalSize
                  else let !nextBuilder =
                             builder <>
                             word64BE (bitStringBuilderChunkContent buff')
                           !totalSize' = totalSize + bitStringMaxLengthBytes
                       in go rest nextBuilder emptyBitStringBuilderChunk totalSize'

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

type family UnsignedDemoteRep i where
  UnsignedDemoteRep Int8  = Word8
  UnsignedDemoteRep Int16 = Word16
  UnsignedDemoteRep Int32 = Word32
  UnsignedDemoteRep Int64 = Word64

-- *** Labbeled Fields

instance
  forall nested l a .
   ( BitStringBuilderHoley (Proxy nested) a )
  => BitStringBuilderHoley (Proxy (LabelF l nested)) a where
  type ToBitStringBuilder (Proxy (LabelF l nested)) a =
    ToBitStringBuilder (Proxy nested) a
  bitStringBuilderHoley _ = bitStringBuilderHoley (Proxy @nested)

-- **** Bool

instance forall f a . (BitRecordFieldSize f ~ 1) =>
  BitStringBuilderHoley (Proxy (f := 'True)) a where
  bitStringBuilderHoley _ = immediate (appendBitString (bitString 1 1))

instance forall f a . (BitRecordFieldSize f ~ 1) =>
  BitStringBuilderHoley (Proxy (f := 'False)) a where
  bitStringBuilderHoley _ = immediate (appendBitString (bitString 1 0))

instance forall a .
  BitStringBuilderHoley (Proxy (MkField 'MkFieldFlag)) a where
  type ToBitStringBuilder (Proxy (MkField 'MkFieldFlag)) a = Bool -> a
  bitStringBuilderHoley _ =
    indirect (appendBitString . bitString 1 . (\ !t -> if t then 1 else 0))

-- **** Bits

instance forall (s :: Nat) a . (KnownChunkSize s) =>
  BitStringBuilderHoley (Proxy (MkField ('MkFieldBits :: BitField (B s) Nat s))) a where
  type ToBitStringBuilder (Proxy (MkField ('MkFieldBits :: BitField (B s) Nat s))) a = B s -> a
  bitStringBuilderHoley _ = indirect (appendBitString . bitStringProxyLength (Proxy @s) . unB)

-- **** Naturals

instance forall a .
  BitStringBuilderHoley (Proxy (MkField 'MkFieldU64)) a where
  type ToBitStringBuilder (Proxy (MkField 'MkFieldU64)) a = Word64 -> a
  bitStringBuilderHoley _ = indirect (appendBitString . bitString 64)

instance forall a .
  BitStringBuilderHoley (Proxy (MkField 'MkFieldU32)) a where
  type ToBitStringBuilder (Proxy (MkField 'MkFieldU32)) a = Word32 -> a
  bitStringBuilderHoley _ = indirect (appendBitString . bitString 32 . fromIntegral)

instance forall a .
  BitStringBuilderHoley (Proxy (MkField 'MkFieldU16)) a where
  type ToBitStringBuilder (Proxy (MkField 'MkFieldU16)) a = Word16 -> a
  bitStringBuilderHoley _ = indirect (appendBitString . bitString 16 . fromIntegral)

instance forall a .
  BitStringBuilderHoley (Proxy (MkField 'MkFieldU8)) a where
  type ToBitStringBuilder (Proxy (MkField 'MkFieldU8)) a = Word8 -> a
  bitStringBuilderHoley _ = indirect (appendBitString . bitString 8 . fromIntegral)

-- **** Signed

instance forall a .
  BitStringBuilderHoley (Proxy (MkField 'MkFieldI64)) a where
  type ToBitStringBuilder (Proxy (MkField 'MkFieldI64)) a = Int64 -> a
  bitStringBuilderHoley _ = indirect (appendBitString . bitString 64 . fromIntegral @Int64 @Word64)

instance forall a .
  BitStringBuilderHoley (Proxy (MkField 'MkFieldI32)) a where
  type ToBitStringBuilder (Proxy (MkField 'MkFieldI32)) a = Int32 -> a
  bitStringBuilderHoley _ = indirect (appendBitString . bitString 32 . fromIntegral . fromIntegral @Int32 @Word32)

instance forall a .
  BitStringBuilderHoley (Proxy (MkField 'MkFieldI16)) a where
  type ToBitStringBuilder (Proxy (MkField 'MkFieldI16)) a = Int16 -> a
  bitStringBuilderHoley _ = indirect (appendBitString . bitString 16 . fromIntegral . fromIntegral @Int16 @Word16)

instance forall a .
  BitStringBuilderHoley (Proxy (MkField 'MkFieldI8)) a where
  type ToBitStringBuilder (Proxy (MkField 'MkFieldI8)) a = Int8 -> a
  bitStringBuilderHoley _ = indirect (appendBitString . bitString 8 . fromIntegral . fromIntegral @Int8 @Word8)


instance forall (f :: IsA (BitRecordField (t :: BitField rt Nat len))) (v :: Nat) a . (KnownNat v, BitStringBuilderHoley (Proxy f) a, ToBitStringBuilder (Proxy f) a ~ (rt -> a), Num rt) =>
  BitStringBuilderHoley (Proxy (f := v)) a where
  bitStringBuilderHoley _ = applyHoley (bitStringBuilderHoley (Proxy @f)) (fromIntegral (natVal (Proxy @v)))

instance forall v f a x . (KnownNat v, BitStringBuilderHoley (Proxy f) a, ToBitStringBuilder (Proxy f) a ~ (x -> a), Num x) =>
  BitStringBuilderHoley (Proxy (f := ('PositiveNat v))) a where
  bitStringBuilderHoley _ =  applyHoley (bitStringBuilderHoley (Proxy @f)) (fromIntegral (natVal (Proxy @v)))


instance forall v f a x . (KnownNat v, BitStringBuilderHoley (Proxy f) a, ToBitStringBuilder (Proxy f) a ~ (x -> a), Num x) =>
  BitStringBuilderHoley (Proxy (f := ('NegativeNat v))) a where
  bitStringBuilderHoley _ = applyHoley (bitStringBuilderHoley (Proxy @f)) (fromIntegral (-1 * (natVal (Proxy @v))))

-- ** 'BitRecord' instances

instance forall (r :: IsA BitRecord) a . BitStringBuilderHoley (Proxy (Eval r)) a =>
  BitStringBuilderHoley (Proxy r) a where
  type ToBitStringBuilder (Proxy r) a =
    ToBitStringBuilder (Proxy (Eval r)) a
  bitStringBuilderHoley _ = bitStringBuilderHoley (Proxy @(Eval r))

-- *** 'BitRecordMember'

instance forall f a . BitStringBuilderHoley (Proxy f) a => BitStringBuilderHoley (Proxy ('BitRecordMember f)) a where
  type ToBitStringBuilder (Proxy ('BitRecordMember f)) a = ToBitStringBuilder (Proxy f) a
  bitStringBuilderHoley _ = bitStringBuilderHoley (Proxy @f)

-- *** 'AppendedBitRecords'

instance forall l r a .
  (BitStringBuilderHoley (Proxy l) (ToBitStringBuilder (Proxy r) a)
  , BitStringBuilderHoley (Proxy r) a)
   => BitStringBuilderHoley (Proxy (l ':>: r)) a where
  type ToBitStringBuilder (Proxy (l ':>: r)) a =
    ToBitStringBuilder (Proxy l) (ToBitStringBuilder (Proxy r) a)
  bitStringBuilderHoley _ = bitStringBuilderHoley (Proxy @l) . bitStringBuilderHoley (Proxy @r)

-- *** 'EmptyBitRecord' and '...Pretty'

instance forall d r a . BitStringBuilderHoley (Proxy r) a =>
  BitStringBuilderHoley (Proxy ('BitRecordDocNested d r)) a where
  type ToBitStringBuilder (Proxy ('BitRecordDocNested d r)) a =
    ToBitStringBuilder (Proxy r) a
  bitStringBuilderHoley _ = bitStringBuilderHoley (Proxy @r)

instance BitStringBuilderHoley (Proxy ('BitRecordDoc d)) a where
  bitStringBuilderHoley _ = id

instance BitStringBuilderHoley (Proxy 'EmptyBitRecord) a where
  bitStringBuilderHoley _ = id

-- ** Tracing/Debug Printing

-- | Print a 'Builder' to a space seperated series of hexa-decimal bytes.
printBuilder :: Builder -> String
printBuilder b =
  ("<< " ++) $
  (++ " >>") $ unwords $ printf "%0.2x" <$> B.unpack (toLazyByteString b)

bitStringPrinter
  :: BitStringBuilderHoley a String
  => a -> ToBitStringBuilder a String
bitStringPrinter =
  runHoley . hoistM (printBuilder . runBitStringBuilder) . bitStringBuilderHoley
