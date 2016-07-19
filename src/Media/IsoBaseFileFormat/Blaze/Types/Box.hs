-- | Definition of the most basic elements in an ISOBMFF file /boxes/.
-- See Chapter 4 in the standard document. Since the standard
module Media.IsoBaseFileFormat.Blaze.Types.Box where

import Data.Bits
import Data.ByteString.Builder
import Data.Monoid
import Data.Proxy
import Data.Word
import GHC.TypeLits

-- * Common boxes

-- xxx = showBox (Proxy :: Proxy SvensBox) (Extend (FullBoxHeader undefined undefined) (Svens "hello"))

mkSvensBox :: Word64 -> SvensBox

mkSvensBox str =
  Box (FullBoxHeader (BoxVersion 3)
                     (BoxFlags 0) <|
       Svens str)

type SvensBox = Box "sven"

instance IsBox "sven" where
  type BoxContent "sven" = FullBox Svens
  boxType _ = FourCc ('s', 'v', 'e', 'n')

data Svens = Svens Word64

instance IsBoxContent Svens where
  boxSize (Svens s) = BoxSize s
  boxBuilder (Svens s) = word64BE s

-- * Abstract Boxes

-- | A `Box` with /version/ and /branding/ information
type FullBox t = Extend FullBoxHeader t

-- | The additional header with /version/ and /branding/ information
data FullBoxHeader = FullBoxHeader BoxVersion (BoxFlags 24)

instance IsBoxContent FullBoxHeader where
  boxSize (FullBoxHeader _ f) = 4 + boxSize f
  boxBuilder (FullBoxHeader (BoxVersion v) f) =
      word8 v <> boxBuilder f

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
boxFlagBitMask :: KnownNat bits => BoxFlags bits -> Integer
boxFlagBitMask px = 2 ^ natVal px - 1

-- | Internal function that masks-out all bits higher than 'bits'.
cropBits :: KnownNat bits => BoxFlags bits -> BoxFlags bits
cropBits f@(BoxFlags b) = BoxFlags (b .&. boxFlagBitMask f)

-- | Get the number of bytes required to store a number of bits.
instance KnownNat bits => IsBoxContent (BoxFlags bits) where
  boxSize f =
    let minBytes = fromInteger $ natVal f `div` 8
        modBytes = fromInteger $ natVal f `mod` 8
    in BoxSize $ minBytes + signum modBytes
  boxBuilder f@(BoxFlags b) =
    let bytes = let (BoxSize bytes') = boxSize f in fromIntegral bytes'
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

-- | A type that wraps the contents of a box.
data Box t where
  Box :: IsBox b => BoxContent b -> Box b

instance (IsBox t) => IsBox (Box t) where
  type BoxContent (Box t) = Box (BoxContent t)
  boxType = boxType

instance IsBoxContent t => IsBoxContent (Box t) where
  boxBuilder b@(Box cnt) = sFix <> tFix <> sExt <> tExt <> boxBuilder cnt
    where s = boxSize b
          t = boxType b
          sFix = boxBuilder s
          sExt = boxBuilder (BoxSizeExtension s)
          tFix = boxBuilder t
          tExt = boxBuilder (BoxTypeExtension t)
  boxSize b@(Box cnt) = sPayload + boxSize (BoxSizeExtension sPayload)
    where sPayload =
            boxSize sPayload + boxSize t + boxSize (BoxTypeExtension t)
          t = boxType b

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
data BoxSize = UnlimitedSize | BoxSize Word64
  deriving (Show, Eq)

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
  fromInteger 1 = UnlimitedSize
  fromInteger n = BoxSize $ fromInteger n

-- | The 'BoxSize' can be > 2^32 in which case an 'BoxSizeExtension' must be
-- added after the type field.
data BoxSizeExtension = BoxSizeExtension BoxSize

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
data BoxType =
  -- | `FourCc` can be used as @boxType@ in `Box`, standard four letter character
  -- code, e.g. @ftyp@
  FourCc (Char,Char,Char,Char)
  -- | CustomBoxType defines custom @boxType@s in `Box`es.
  | CustomBoxType UUID
  deriving (Show, Eq)

newtype UUID =
  UUID String -- TODO
  deriving (Show, Eq)

instance IsBoxContent BoxType where
  boxSize _ = 4
  boxBuilder t =
                  case t of
                    FourCc x -> putFourCc x
                    CustomBoxType (UUID u) ->
                        putFourCc ('u','u','i','d')
             where
                putFourCc (a,b,c,d) = putW a <> putW b <> putW c <> putW d
                putW = word8 . fromIntegral . fromEnum

-- | When using custom types extra data must be written after the extra size
-- information. Since the box type and the optional custom box type are not
-- guaranteed to be consequtive, this type handles the /second/ part seperately.
data BoxTypeExtension = BoxTypeExtension BoxType

instance IsBoxContent BoxTypeExtension where
  boxSize (BoxTypeExtension (FourCc _)) = 0
  boxSize (BoxTypeExtension (CustomBoxType _)) = 16 * 4


-- * Box inheritance/extension

-- | A meta box that mimic object oriented programming inheritance. The first
-- parameter is the /base class/ and should be instance of 'IsBoxContent' and
-- the second parameter is the /derived class/. This can represent
-- 'IsBoxContent' as well as 'IsBox' instances.
data Extend a b = Extend a b

-- | A nice operator to for 'Extend'.
(<|) :: a -> b -> Extend a b
(<|) = Extend

instance (IsBoxContent p,IsBoxContent c) => IsBoxContent (Extend p c) where
  boxSize (Extend p c) = boxSize p + boxSize c

-- | While 'Extend' is an instance of 'IsBoxContent' the (uninhabited) promoted
-- type has an 'IsBox' instance with the content type set to the unpromoted
-- 'Extend'.
instance (IsBoxContent parent, IsBox child) => IsBox ('Extend parent child) where
  type BoxContent ('Extend parent child) = Extend parent (BoxContent child)
  boxType _ = boxType (Proxy :: Proxy child)

-- * Box type classes and functions

-- | Types that are boxes. A box has a box type which is
-- identified on the type level, hence the use of the proxy.
class IsBoxContent (BoxContent a) => IsBox a where
  type BoxContent a
  -- | Type-level /box types/ can be converted into value level box types.
  boxType :: proxy a -> BoxType

-- | Types that go into a box. A box content is a piece of data that can be
-- reused in different instances of 'IsBox'. It has no 'BoxType' and hence
-- defines no box.
class IsBoxContent a where
  boxSize :: a -> BoxSize
  boxBuilder :: a -> Builder
