-- | Mini EDSL for labelled box fields. The boxfields can be 'Scalar' or
-- 'ScalarArray's.
module Data.ByteString.IsoBaseFileFormat.Boxes.BoxFields
       where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box
import Data.Int
import Data.Maybe
import Text.Printf
import Data.Singletons
import Data.Singletons.Prelude.List
import qualified Data.Vector.Sized as Vec

-- * Convenient API
type U64 label = Scalar label Word64

type I64 label = Scalar label Int64

u64 :: Word64 -> U64 label
u64 = Scalar

i64 :: Int64 -> I64 label
i64 = Scalar

type U32 label = Scalar label Word32

type I32 label = Scalar label Int32

u32 :: Word32 -> U32 label
u32 = Scalar

i32 :: Int32 -> I32 label
i32 = Scalar

type U16 label = Scalar label Word16

type I16 label = Scalar label Int16

u16 :: Word16 -> U16 label
u16 = Scalar

i16 :: Int16 -> I16 label
i16 = Scalar

type U8 label = Scalar label Word8

type I8 label = Scalar label Int8

u8 :: Word8 -> U8 label
u8 = Scalar

i8 :: Int8 -> I8 label
i8 = Scalar

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

-- * Conversion from type-level numbers and lists to values
class FromTypeLit o v  where
  fromTypeLit :: proxy o v -> o

instance (SingI arr,Num o,SingKind [Nat],KnownNat len,len ~ Length arr) => FromTypeLit (ScalarArray label len o) (arr :: [Nat]) where
  fromTypeLit _ =
    let s = sing :: Sing arr
        vs :: [Integer]
        vs = fromSing s
        vs' :: [o]
        vs' = fromIntegral <$> vs
    in ScalarArray (fromJust (Vec.fromList vs'))

-- * Scalar box fields
newtype Scalar (label :: k) scalartype =
  Scalar scalartype deriving (Show, Read, Ord, Eq, Num)

instance IsBoxContent (Scalar label Word8) where
  boxSize _ = 1
  boxBuilder (Scalar v) = word8 v

instance IsBoxContent (Scalar label Word16) where
  boxSize _ = 2
  boxBuilder (Scalar v) = word16BE v

instance IsBoxContent (Scalar label Word32) where
  boxSize _ = 4
  boxBuilder (Scalar v) = word32BE v

instance IsBoxContent (Scalar label Word64) where
  boxSize _ = 8
  boxBuilder (Scalar v) = word64BE v

instance IsBoxContent (Scalar label Int8) where
  boxSize _ = 1
  boxBuilder (Scalar v) = int8 v

instance IsBoxContent (Scalar label Int16) where
  boxSize _ = 2
  boxBuilder (Scalar v) = int16BE v

instance IsBoxContent (Scalar label Int32) where
  boxSize _ = 4
  boxBuilder (Scalar v) = int32BE v

instance IsBoxContent (Scalar label Int64) where
  boxSize _ = 8
  boxBuilder (Scalar v) = int64BE v

instance (KnownNat scalar,Num o) => FromTypeLit (Scalar label o) scalar where
  fromTypeLit _ = Scalar $ fromIntegral $ natVal (Proxy :: Proxy scalar)

-- * Scalar array
newtype ScalarArray (label :: k) (len :: Nat) o where
        ScalarArray :: Vec.Vector n o -> ScalarArray label n o

instance (Num o,IsBoxContent (Scalar label o),KnownNat (len :: Nat)) => IsBoxContent (ScalarArray label len o) where
  boxSize (ScalarArray vec) =
    fromIntegral (Vec.length vec) * boxSize (Scalar 0 :: Scalar label o)
  boxBuilder (ScalarArray vec) =
    Vec.foldl' mappend
               mempty
               (Vec.map (boxBuilder . mkScalar) vec)
    where mkScalar :: o -> Scalar label o
          mkScalar = Scalar

fromList :: forall label n o.
            (KnownSymbol label,KnownNat n)
         => [o] -> ScalarArray label n o
fromList l =
  ScalarArray $
  case Vec.fromList l of
    Nothing ->
      error $
      printf "Invalid number of array elements for array %s. Got length: %d elments, expected %d."
             (show (symbolVal (Proxy :: Proxy label)))
             (natVal (Proxy :: Proxy n))
             (length l)
    Just v -> v

-- * Constant fields with default values
data Constant o v where
        Constant :: Constant o v

instance (IsBoxContent o,FromTypeLit o v) => IsBoxContent (Constant o v) where
  boxSize = boxSize . fromTypeLit
  boxBuilder = boxBuilder . fromTypeLit

-- * Fields with default values that can be overriden with custom value
data Template o v where
        Default :: Template o v
        Custom :: o -> Template o v

instance (IsBoxContent o,FromTypeLit o v) => IsBoxContent (Template o v) where
  boxSize = boxSize . fromTypeLit
  boxBuilder d@Default = boxBuilder $ fromTypeLit d
  boxBuilder (Custom o) = boxBuilder o
