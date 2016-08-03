-- | Full Boxes
module Data.ByteString.IsoBaseFileFormat.Boxes.FullBox
       (FullBox(..), fullBox, BoxFlags(..))
       where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box

-- | A 'FullBox' contains an extra version and a flags field. In this
-- implementation it is wrapped around the rest of the box content. This
-- enforces that the 'FullBox' header fields are always at the beginning - at
-- least as long as this module hides the 'FullBox' constructor ;)
data FullBox t (version :: Nat) where
  FullBox :: (KnownNat version, IsBox t)
          => BoxFlags 24
          -> BoxContent t
          -> FullBox t version

instance (KnownNat version, IsBox t, Default (BoxContent t))
  => Default (FullBox t version) where
  def = FullBox 0 def

instance (KnownNat v, IsBox t) => IsBox (FullBox t v) where
  type BoxContent (FullBox t v) = FullBox t v

type instance BoxTypeSymbol (FullBox t v) = BoxTypeSymbol t

instance (IsBox t, KnownNat v) => IsBoxContent (FullBox t v) where
  boxSize (FullBox f c) = 1 + boxSize f + boxSize c
  boxBuilder (FullBox f c) =
       word8 (fromIntegral (natVal (Proxy :: Proxy v)))
    <> boxBuilder f
    <> boxBuilder c

-- | Create a 'FullBox' from a 'BoxVersion' and 'BoxFlags'
fullBox
  :: (KnownNat v, IsBox t)
  => BoxFlags 24 -> BoxContent t -> Box (FullBox t v)
fullBox f c = Box (FullBox f c)

-- | In addition to a version there can be 24 bits for custom flags etc in
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
