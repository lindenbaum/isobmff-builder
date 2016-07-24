-- | Full Boxes
module Data.ByteString.IsoBaseFileFormat.Boxes.FullBox
       (FullBox(), fullBox, closedFullBox, BoxVersion, BoxFlags(..),
        Versioned(..))
       where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box
import Data.ByteString.IsoBaseFileFormat.Boxes.BoxFields
import Data.Default

-- | A 'FullBox' contains an extra version and a flags field. In this
-- implementation it is wrapped around the rest of the box content. This
-- enforces that the 'FullBox' header fields are always at the beginning - at
-- least as long as this module hides the 'FullBox' constructor ;)
data FullBox version t where
        FullBox ::
          BoxVersion version -> BoxFlags 24 -> t -> FullBox version t

instance (KnownNat version,IsBoxContent t) => IsBoxContent (FullBox version t) where
  boxSize (FullBox _ f c) = 1 + boxSize f + boxSize c
  boxBuilder (FullBox v f c) = boxBuilder v <> boxBuilder f <> boxBuilder c

-- | Create a 'FullBox' from a 'BoxVersion' and 'BoxFlags'
fullBox
  :: (IsBoxType' t,ValidBoxes t ts,BoxContent t ~ FullBox version c)
  => BoxVersion version -> BoxFlags 24 -> c -> Boxes ts -> Box t
fullBox version fs cnt = Box (FullBox version fs cnt)

-- | Create a 'FullBox' from a 'BoxVersion' and 'BoxFlags' without nested boxes.
closedFullBox
  :: (IsBoxType' t,ValidBoxes t '[],BoxContent t ~ FullBox version c)
  => BoxVersion version -> BoxFlags 24 -> c -> Box t
closedFullBox version fs cnt = closedBox (FullBox version fs cnt)

-- | The box version (in a 'FullBox') is a single byte
type BoxVersion v = Template (U8 "fullbox-version") v

-- | Two alternative representations based on a /version/ index.
--   Use this for box content that can be either 32 or 64 bit.
data Versioned v0 v1 (version :: Nat) where
        V0 :: IsBoxContent v0 => v0 -> Versioned v0 v1 0
        V1 :: IsBoxContent v1 => v1 -> Versioned v0 v1 1

instance (version ~ 0,IsBoxContent v0,Default v0) => Default (Versioned v0 v1 (version :: Nat)) where
  def = V0 def

instance IsBoxContent (Versioned v0 v1 version) where
  boxSize (V0 c) = boxSize c
  boxSize (V1 c) = boxSize c
  boxBuilder (V0 c) = boxBuilder c
  boxBuilder (V1 c) = boxBuilder c

-- | In addition to a 'BoxVersion' there can be 24 bits for custom flags etc in
-- a 'FullBox'.
newtype BoxFlags bits =
  BoxFlags Integer
  deriving (Eq,Show,Num)

-- | Internal function that creates a bit mask with all bits in a 'BoxFlags' set
-- to 1.
boxFlagBitMask :: KnownNat bits
               => BoxFlags bits -> Integer
boxFlagBitMask px = 2 ^ natVal px - 1

-- | Internal function that masks-out all bits higher than 'bits'.
cropBits :: KnownNat bits
         => BoxFlags bits -> BoxFlags bits
cropBits f@(BoxFlags b) = BoxFlags (b .&. boxFlagBitMask f)

-- | Get the number of bytes required to store a number of bits.
instance KnownNat bits => IsBoxContent (BoxFlags bits) where
  boxSize f =
    let minBytes = fromInteger $ natVal f `div` 8
        modBytes = fromInteger $ natVal f `mod` 8
    in BoxSize $ minBytes + signum modBytes
  boxBuilder f@(BoxFlags b) =
    let bytes =
          let (BoxSize bytes') = boxSize f
          in fromIntegral bytes'
        wordSeq n
          | n <= bytes =
            word8 (fromIntegral (shiftR b ((bytes - n) * 8) .&. 255)) <>
            wordSeq (n + 1)
          | otherwise = mempty
    in wordSeq 1

instance KnownNat bits => Bits (BoxFlags bits) where
  (.&.) (BoxFlags l) (BoxFlags r) = cropBits $ BoxFlags $ l .&. r
  (.|.) (BoxFlags l) (BoxFlags r) = cropBits $ BoxFlags $ l .&. r
  xor (BoxFlags l) (BoxFlags r) = cropBits $ BoxFlags $ xor l r
  complement (BoxFlags x) = cropBits $ BoxFlags $ complement x
  shift (BoxFlags x) = cropBits . BoxFlags . shift x
  rotateL = error "TODO rotateL"
  rotateR = error "TODO rotateR"
  bitSize = fromInteger . natVal
  bitSizeMaybe = Just . fromInteger . natVal
  isSigned _ = False
  testBit f n =
    let (BoxFlags b) = cropBits f
    in testBit b n
  bit = cropBits . BoxFlags . bit
  popCount f =
    let (BoxFlags b) = cropBits f
    in popCount b
  zeroBits = BoxFlags 0
