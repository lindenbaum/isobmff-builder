{-# LANGUAGE UndecidableInstances #-}
-- | Mini EDSL for labelled box fields. The boxfields can be 'Scalar' or
-- 'ScalarArray's.
module Data.ByteString.IsoBaseFileFormat.Util.BoxFields
       where

import Data.ByteString.IsoBaseFileFormat.Box
import Data.ByteString.IsoBaseFileFormat.ReExports
import Data.Singletons

import Data.Singletons.Prelude.List
import qualified Data.Vector as Vec
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B


-- * Scalar box fields

type U64 label = Scalar Word64 label

type I64 label = Scalar Int64 label

u64 :: Word64 -> U64 label
u64 = Scalar

i64 :: Int64 -> I64 label
i64 = Scalar

type U32 label = Scalar Word32 label
type I32 label = Scalar Int32 label

u32 :: Word32 -> U32 label
u32 = Scalar

i32 :: Int32 -> I32 label
i32 = Scalar

type U16 label = Scalar Word16 label

type I16 label = Scalar Int16 label

u16 :: Word16 -> U16 label
u16 = Scalar

i16 :: Int16 -> I16 label
i16 = Scalar

type U8 label = Scalar Word8 label

type I8 label = Scalar Int8 label

u8 :: Word8 -> U8 label
u8 = Scalar

i8 :: Int8 -> I8 label
i8 = Scalar

-- | A numeric box field with a type level label. Note that it has a 'Num'
-- instance. Use the type aliases above, e.g.
-- 'U8','I8','U16','I16','U32','I32','U64','I64' from above. Use either the
-- smart constructors, e.g. 'u8','i8','u16','i16','u32','i32','u64','i64' or the
-- 'Num' instance, whereas the constructors might give a bit more safety.
newtype Scalar scalartype (label :: k) =
  Scalar {fromScalar :: scalartype}
  deriving (Show, Read, Ord, Eq, Num, Default)

-- | Relabel a scalar value, e.g. convert a @Scalar U32 "foo"@ to a @Scalar U32
-- "bar"@.
relabelScalar :: Scalar t l -> Scalar t l'
relabelScalar (Scalar x) = Scalar x

instance IsBoxContent (Scalar Word8 label) where
  boxSize _ = 1
  boxBuilder (Scalar v) = word8 v

instance IsBoxContent (Scalar Word16 label) where
  boxSize _ = 2
  boxBuilder (Scalar v) = word16BE v

instance IsBoxContent (Scalar Word32 label) where
  boxSize _ = 4
  boxBuilder (Scalar v) = word32BE v

instance IsBoxContent (Scalar Word64 label) where
  boxSize _ = 8
  boxBuilder (Scalar v) = word64BE v

instance IsBoxContent (Scalar Int8 label) where
  boxSize _ = 1
  boxBuilder (Scalar v) = int8 v

instance IsBoxContent (Scalar Int16 label) where
  boxSize _ = 2
  boxBuilder (Scalar v) = int16BE v

instance IsBoxContent (Scalar Int32 label) where
  boxSize _ = 4
  boxBuilder (Scalar v) = int32BE v

instance IsBoxContent (Scalar Int64 label) where
  boxSize _ = 8
  boxBuilder (Scalar v) = int64BE v

instance (KnownNat scalar,Num o) => FromTypeLit (Scalar o label) scalar where
  fromTypeLit _ = Scalar $ fromIntegral $ natVal (Proxy :: Proxy scalar)

-- * Array fields

type U64Arr label size = ScalarArray label size Word64

u64Arr :: (KnownNat size,KnownSymbol label)
       => [Word64] -> U64Arr label size
u64Arr = fromList

type I64Arr label size = ScalarArray label size Int64

i64Arr :: (KnownNat size,KnownSymbol label)
       => [Int64] -> I64Arr label size
i64Arr = fromList

type U32Arr label size = ScalarArray label size Word32

u32Arr :: (KnownNat size,KnownSymbol label)
       => [Word32] -> U32Arr label size
u32Arr = fromList

type I32Arr label size = ScalarArray label size Int32

i32Arr :: (KnownNat size,KnownSymbol label)
       => [Int32] -> I32Arr label size
i32Arr = fromList

type U16Arr label size = ScalarArray label size Word16

u16Arr :: (KnownNat size,KnownSymbol label)
       => [Word16] -> U16Arr label size
u16Arr = fromList

type I16Arr label size = ScalarArray label size Int16

i16Arr :: (KnownNat size,KnownSymbol label)
       => [Int16] -> I16Arr label size
i16Arr = fromList

type U8Arr label size = ScalarArray label size Word8

u8Arr :: (KnownNat size,KnownSymbol label)
      => [Word8] -> U8Arr label size
u8Arr = fromList

type I8Arr label size = ScalarArray label size Int8

i8Arr :: (KnownNat size,KnownSymbol label)
      => [Int8] -> I8Arr label size
i8Arr = fromList

-- | A box field that is an array of 'Scalar's with a type level label. Use the
-- type aliases, e.g.
-- 'U8Arr','I8Arr','U16Arr','I16Arr','U32Arr','I32Arr','U64Arr','I64Arr' from
-- above. Use the smart constructors, e.g.
-- 'u8Arr','i8Arr','u16Arr','i16Arr','u32Arr','i32Arr','u64Arr','i64Arr' .
newtype ScalarArray (label :: k) (len :: Nat) o where
        ScalarArray :: Vec.Vector o -> ScalarArray label n o
        deriving (Show,Eq)

instance (Default o,KnownNat (len :: Nat))
  => Default (ScalarArray label len o) where
  def = ScalarArray $ Vec.replicate (fromIntegral (natVal (Proxy @len))) def

instance (Num o,IsBoxContent (Scalar o label))
  => IsBoxContent (ScalarArray label len o) where
  boxSize (ScalarArray vec) =
    fromIntegral (Vec.length vec) * boxSize (Scalar 0 :: Scalar o label)
  boxBuilder (ScalarArray vec) =
    Vec.foldl' mappend
               mempty
               (Vec.map (boxBuilder . mkScalar) vec)
    where mkScalar :: o -> Scalar o label
          mkScalar = Scalar

-- | Internal function
fromList :: forall label n o.
            (KnownSymbol label,KnownNat n)
         => [o] -> ScalarArray label n o
fromList l =
  ScalarArray $
  if natVal (Proxy @n) /= toInteger (length l)
  then
    error $ printf "Invalid number of array elements for array %s. Got length: %d elments, expected %d."
             (show (symbolVal (Proxy :: Proxy label)))
             (length l)
             (natVal (Proxy :: Proxy n))
    else
      Vec.fromList l

-- * Constant fields

-- | Wrapper around a field, e.g. a 'Scalar' or 'ScalarArray', with a type level
-- value. The wrapped content must implement 'FromTypeLit'. To get the value of
-- a 'Constant'  use 'fromTypeLit'.
data Constant o v where
        Constant :: Constant o v

instance (IsBoxContent o,FromTypeLit o v) => IsBoxContent (Constant o v) where
  boxSize = boxSize . fromTypeLit
  boxBuilder = boxBuilder . fromTypeLit

instance Default (Constant o v) where
  def = Constant

-- * Template Fields

-- | Fields with default values that can be overriden with custom value. Like
-- 'Constant' this is a wrapper around a field, e.g. a 'Scalar' or
-- 'ScalarArray', with a type level default value. The wrapped content must
-- implement 'FromTypeLit'.
data Template o v where
        Template :: Template o v
        Custom :: !o -> Template o v

instance Default (Template o v) where
  def = Template

-- | Get a value from a 'Template'.
templateValue :: FromTypeLit o v => Template o v -> o
templateValue d@Template = fromTypeLit d
templateValue (Custom v) = v

instance (IsBoxContent o,FromTypeLit o v) => IsBoxContent (Template o v) where
  boxSize = boxSize . templateValue
  boxBuilder = boxBuilder . templateValue

-- * Conversion from type-level numbers and lists to values

-- | Types that can be constructed from type level value representations.
class FromTypeLit o v  where
  fromTypeLit :: proxy o v -> o

instance forall arr o len (label :: Symbol) .
  (KnownSymbol label,SingI arr,Num o,SingKind [Nat],KnownNat len,len ~ Length arr)
  => FromTypeLit (ScalarArray label len o) (arr :: [Nat]) where
  fromTypeLit _ =
    let s = sing :: Sing arr
        vs :: [Integer]
        vs = fromSing s
        vs' :: [o]
        vs' = fromIntegral <$> vs
    in (fromList vs' :: ScalarArray label len o)

instance KnownSymbol str => FromTypeLit T.Text (str :: Symbol) where
  fromTypeLit = T.pack . symbolVal

-- * String/Text field types

-- | A fixed size string, the first byte is the string length, after the String,
-- the field is padded with @0@ bytes. The string must be in UTF8 format.
newtype FixSizeText (len :: Nat) (label :: Symbol) where
  FixSizeText :: T.Text -> FixSizeText len label

-- | A constraint that matches type level numbers that are valid text sizes for
--  'FixSizeText's.
type IsTextSize len = (KnownNat len, 1 <= len, len <= 255)

instance IsTextSize len => IsBoxContent (FixSizeText len label) where
  boxSize    _               = fromIntegral (natVal (Proxy :: Proxy len))
  boxBuilder (FixSizeText t) =
    let
      -- leave room for the size byte
      maxSize             = fromIntegral (natVal (Proxy :: Proxy len) - 1)
      displayableText     = B.take maxSize (T.encodeUtf8 t)
      displayableTextSize = B.length displayableText
      paddingSize         = max 0 (maxSize - displayableTextSize)
      in word8 (fromIntegral displayableTextSize)
         <> byteString displayableText
         <> fold (replicate paddingSize (word8 0))

instance IsTextSize len => Default (FixSizeText len label) where
  def = FixSizeText ""

-- | Four character strings embedded in a uint32.
newtype U32Text (label :: Symbol) where
  U32Text :: Word32 -> U32Text label

instance IsString (U32Text label) where
  fromString str = U32Text $
    let cw s c = (0xFF .&. fromIntegral (fromEnum c)) `shiftL` s
        in case str of
             []          -> 0x20202020
             [a]         -> cw 24 a .|. 0x00202020
             [a,b]       -> cw 24 a .|. cw 16 b  .|. 0x00002020
             [a,b,c]     -> cw 24 a .|. cw 16 b  .|. cw 8 c  .|. 0x20
             (a:b:c:d:_) -> cw 24 a .|. cw 16 b  .|. cw 8 c  .|. cw 0 d

instance IsBoxContent (U32Text label) where
  boxSize _ = 4
  boxBuilder (U32Text t) = word32BE t

instance KnownSymbol str => FromTypeLit (U32Text label) (str :: Symbol) where
  fromTypeLit = fromString . symbolVal

instance Default (U32Text label) where
  def = U32Text 0x20202020
