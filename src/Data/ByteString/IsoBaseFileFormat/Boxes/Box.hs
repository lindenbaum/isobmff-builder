-- | Definition of the most basic elements in an ISOBMFF file /boxes/.
-- See Chapter 4 in the standard document. Since the standard
module Data.ByteString.IsoBaseFileFormat.Boxes.Box
       (module Data.ByteString.IsoBaseFileFormat.Boxes.Box, module X) where

import Data.Bits as X
import Data.ByteString.Builder as X
import Data.Monoid as X
import Data.Proxy as X
import Data.Word as X
import GHC.TypeLits as X
import Data.String

-- * Common box /combinators/
-- | A `Box` with /version/ and /branding/ information
type FullBox t = Extend FullBoxHeader t

-- | Create a 'FullBox' from a 'FourCc' 'StdType' and the nested box content.
fullBox
  :: IsBoxContent c
  => FourCc -> BoxVersion -> BoxFlags 24 -> c -> Box (FullBox c)
fullBox fcc ver fs cnt = box fcc (Extend (FullBoxHeader ver fs) cnt)

-- | The additional header with /version/ and /branding/ information
data FullBoxHeader =
  FullBoxHeader BoxVersion
                (BoxFlags 24)

instance IsBoxContent FullBoxHeader where
  boxSize (FullBoxHeader _ f) = 1 + boxSize f
  boxBuilder (FullBoxHeader (BoxVersion v) f) = word8 v <> boxBuilder f

-- | The box version (in a 'FullBox') is a single byte
newtype BoxVersion =
  BoxVersion Word8

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
  (.&.) lf@(BoxFlags l) (BoxFlags r) = cropBits $ BoxFlags $ l .&. r
  (.|.) lf@(BoxFlags l) (BoxFlags r) = cropBits $ BoxFlags $ l .&. r
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

-- * /The/ actual Box
-- | Create a 'Box' with a 'StdType' 'FourCc' type.
box :: IsBoxContent c
    => FourCc -> c -> Box c
box fcc cnt = Box (StdType fcc) cnt

-- | A type that wraps the contents of a box and the box type.
data Box a where
        Box :: IsBoxContent c => BoxType -> c -> Box c

instance IsBoxContent a => IsBoxContent (Box a) where
  boxBuilder b@(Box t cnt) = sFix <> tFix <> sExt <> tExt <> boxBuilder cnt
    where s = boxSize b
          sFix = boxBuilder s
          sExt = boxBuilder (BoxSizeExtension s)
          tFix = boxBuilder t
          tExt = boxBuilder (BoxTypeExtension t)
  boxSize b@(Box t cnt) = sPayload + boxSize (BoxSizeExtension sPayload)
    where sPayload =
            boxSize sPayload + boxSize t + boxSize cnt +
            boxSize (BoxTypeExtension t)

-- * Box Meta Data
-- | The box header contains a size and a type. Both can be compact (32bit) or
-- large (64bit size, 17*32bit type).
data BoxHeader =
  BoxHeader BoxSize
            BoxType
  deriving (Show,Eq)

-- | The size of the box. If the size is limited to a (fixed) value, it can be
-- provided as a 'Word64' which will be represented as either a 32bit compact
-- size or as 64 bit /largesize/. If 'UnlimitedSize' is used, the box extends to
-- the end of the file.
data BoxSize
  = UnlimitedSize
  | BoxSize Word64
  deriving (Show,Eq)

instance IsBoxContent BoxSize where
  boxSize _ = BoxSize 4
  boxBuilder UnlimitedSize = word32BE 0
  boxBuilder (BoxSize n) =
    word32BE $
    if n < 2 ^ 32
       then fromIntegral n
       else 1

instance Num BoxSize where
  (+) UnlimitedSize _ = UnlimitedSize
  (+) _ UnlimitedSize = UnlimitedSize
  (+) (BoxSize l) (BoxSize r) = BoxSize (l + r)
  (-) UnlimitedSize _ = UnlimitedSize
  (-) _ UnlimitedSize = UnlimitedSize
  (-) (BoxSize l) (BoxSize r) = BoxSize (l - r)
  (*) UnlimitedSize _ = UnlimitedSize
  (*) _ UnlimitedSize = UnlimitedSize
  (*) (BoxSize l) (BoxSize r) = BoxSize (l * r)
  abs UnlimitedSize = UnlimitedSize
  abs (BoxSize n) = BoxSize (abs n)
  signum UnlimitedSize = UnlimitedSize
  signum (BoxSize n) = BoxSize (signum n)
  fromInteger n = BoxSize $ fromInteger n

-- | The 'BoxSize' can be > 2^32 in which case an 'BoxSizeExtension' must be
-- added after the type field.
data BoxSizeExtension =
  BoxSizeExtension BoxSize

instance IsBoxContent BoxSizeExtension where
  boxBuilder (BoxSizeExtension UnlimitedSize) = mempty
  boxBuilder (BoxSizeExtension (BoxSize n)) =
    if n < 2 ^ 32
       then mempty
       else word64BE n
  boxSize (BoxSizeExtension UnlimitedSize) = 0
  boxSize (BoxSizeExtension (BoxSize n)) =
    BoxSize $
    if n < 2 ^ 32
       then 0
       else 8

-- | A box has a /type/, this is the value level representation for the box type.
data BoxType
  =
    -- | `FourCc` can be used as @boxType@ in `Box`, standard four letter character
    -- code, e.g. @ftyp@
    StdType FourCc
  |
    -- | CustomBoxType defines custom @boxType@s in `Box`es.
    CustomBoxType UUID
  deriving (Show,Eq)

-- | A type containin a printable four letter character code.
newtype FourCc =
  FourCc (Char,Char,Char,Char)
  deriving (Show,Eq)

instance IsString FourCc where
  fromString str
    | length str == 4 =
      let [a,b,c,d] = str
      in FourCc (a,b,c,d)
    | otherwise =
      error ("cannot make a 'FourCc' of a String which isn't exactly 4 bytes long: " ++
             show str ++ " has a length of " ++ show (length str))

instance IsBoxContent FourCc where
  boxSize _ = 4
  boxBuilder (FourCc (a,b,c,d)) = putW a <> putW b <> putW c <> putW d
    where putW = word8 . fromIntegral . fromEnum

newtype UUID =
  UUID String -- TODO
  deriving (Show,Eq)

instance IsBoxContent BoxType where
  boxSize _ = boxSize (FourCc undefined)
  boxBuilder t =
    case t of
      StdType x -> boxBuilder x
      CustomBoxType (UUID u) -> boxBuilder (FourCc ('u','u','i','d'))

-- | When using custom types extra data must be written after the extra size
-- information. Since the box type and the optional custom box type are not
-- guaranteed to be consequtive, this type handles the /second/ part seperately.
data BoxTypeExtension =
  BoxTypeExtension BoxType

instance IsBoxContent BoxTypeExtension where
  boxSize (BoxTypeExtension (StdType _)) = 0
  boxSize (BoxTypeExtension (CustomBoxType _)) = 16 * 4
  boxBuilder (BoxTypeExtension (StdType _)) = mempty
  boxBuilder (BoxTypeExtension (CustomBoxType (UUID str))) =
    mconcat (map (word8 . fromIntegral . fromEnum)
                 (take (16 * 4) str) ++
             repeat (word8 0))

-- * Box inheritance/extension
-- | A meta box that mimic object oriented programming inheritance. The first
-- parameter is the /base class/ and should be instance of 'IsBoxContent' and
-- the second parameter is the /derived class/. This can represent
-- 'IsBoxContent' as well as 'IsBox' instances.
data Extend a b =
  Extend a
         b

-- | A nice operator to for 'Extend'.
(<|) :: a -> b -> Extend a b
(<|) = Extend

instance (IsBoxContent p,IsBoxContent c) => IsBoxContent (Extend p c) where
  boxSize (Extend p c) = boxSize p + boxSize c
  boxBuilder (Extend p c) = boxBuilder p <> boxBuilder c

-- * Box type classes and functions
-- | Types that go into a box. A box content is a piece of data that can be
-- reused in different instances of 'IsBox'. It has no 'BoxType' and hence
-- defines no box.
class IsBoxContent a  where
  boxSize :: a -> BoxSize
  boxBuilder :: a -> Builder
