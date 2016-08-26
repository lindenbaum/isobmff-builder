{-# LANGUAGE UndecidableInstances #-}
module Data.Type.BitRecords.Core where

import Data.Bits
import Data.Int
import Data.Kind (Type, Constraint)
import Data.Proxy
import Data.Type.BitRecords.Arithmetic
import Data.Type.Bool
import Data.Type.Equality
import Data.Type.Pretty
import Data.Word
import GHC.TypeLits
import Test.TypeSpecCrazy


-- * Fields

-- | Define a field with a size
data Field :: Nat -> Type

-- | Alias for a single bit field
type Flag = Field 1

-- | A Flag (1-bit) that is true if the type level maybe is 'Just'.
data FlagJust :: Maybe a -> Type

-- | A Flag (1-bit) that is true if the type level maybe is 'Nothing'.
data FlagNothing :: Maybe a -> Type

-- | A field that renders to the length of a 'SizedString' using the given
-- word type for the size.
data ToStringLength :: Type -> Type -> Type

type FieldPosition = (Nat, Nat)

-- ** Accessors

type family FieldRep (r :: rk)
type instance FieldRep (Field (n :: Nat)) = Word64
type instance FieldRep (f := v) = FieldRep f
type instance FieldRep Word64 = Word64
type instance FieldRep Word32 = Word32
type instance FieldRep Word16 = Word16
type instance FieldRep Word8 = Word8
type instance FieldRep Int64 = Int64
type instance FieldRep Int32 = Int32
type instance FieldRep Int16 = Int16
type instance FieldRep Int8 = Int8
type instance FieldRep Bool = Bool

type family GetFieldSize (r :: rk) :: Nat
type instance GetFieldSize (Field (n :: Nat)) = n
type instance GetFieldSize (f := v) = GetFieldSize f
type instance GetFieldSize Word64 = 64
type instance GetFieldSize Word32 = 32
type instance GetFieldSize Word16 = 16
type instance GetFieldSize Word8 = 8
type instance GetFieldSize Int64 = 64
type instance GetFieldSize Int32 = 32
type instance GetFieldSize Int16 = 16
type instance GetFieldSize Int8 = 8
type instance GetFieldSize Bool = 1
type instance GetFieldSize (bool :: Bool) = 1
type instance GetFieldSize (FlagJust (k :: Maybe t)) = 1
type instance GetFieldSize (FlagNothing (k :: Maybe t)) = 1
type instance GetFieldSize (sizeField :: SizeField) = GetSizeFieldFieldSize sizeField

type family GetSizeFieldFieldSize (sizeField :: SizeField) :: Nat where
  GetSizeFieldFieldSize 'NoSizeField = 0
  GetSizeFieldFieldSize 'SizeField8 = 8
  GetSizeFieldFieldSize 'SizeField16 = 16
  GetSizeFieldFieldSize 'SizeField32 = 32


-- * Records

-- | Combine two fields to form a new field.
data (:>:) :: Type -> k -> Type
infixl 3 :>:

-- | A field with a name
data (:=>) :: label -> Type -> Type where
infixr 5 :=>

-- | A field with a constant fixed value
data (:=) :: Type -> k -> Type
infixr 6 :=

-- | Create a negative type level value for use in ':='
data Negative :: Nat -> Type

-- ** Arrays/Repitition

-- | Anything countable prefixed with a size field of customizable width.
data Sized :: SizeField -> k -> Type

-- | For something to be augmented by a size field there must be an instance of
-- this family to generate the value of the size field, e.g. by counting the
-- elements.
type family SizeFieldValue (c :: k) :: Nat

type instance SizeFieldValue (x :: [k]) = CountListElements x
type instance SizeFieldValue (RecArray t n) = n

type family CountListElements (x :: [k]) :: Nat where
  CountListElements '[] = 0
  CountListElements (x ': xs) = 1 + CountListElements xs

-- | Indicate the size of a length field, e.g. for 'SizedString' or 'Sized'
data SizeField = NoSizeField | SizeField8 | SizeField16 | SizeField32

-- | An array of records with a fixed number of elements, NOTE: this type is
-- actually not really necessary since 'ReplicateRecord' exists, but this allows
-- to have a different 'showRecord' output.
data RecArray :: k -> Nat -> Type

type r ^^ n = RecArray r n
infixr 8 ^^

-- | Repeat a bit record @n@ times.
type family
  ReplicateRecord (n :: Nat) (r :: k) :: [k] where
  ReplicateRecord 0 r = '[]
  ReplicateRecord n r = r ': ReplicateRecord (n - 1) r

-- ** BitRecord Accessor

-- | Return the size of the record.
getRecordSizeFromProxy
  :: forall px rec . KnownNat (GetRecordSize rec) => px rec -> Integer
getRecordSizeFromProxy _ = natVal (Proxy :: Proxy (GetRecordSize rec))

type family
  GetRecordSize (r :: rk) :: Nat where
  GetRecordSize (l :>: r)      = GetRecordSize l + GetRecordSize r
  GetRecordSize (RecArray r n) = n               * GetRecordSize r
  GetRecordSize (Sized s r)    = GetFieldSize s + GetRecordSize r
  GetRecordSize (x :: [k])    = GetRecordListSize x
  GetRecordSize (x :: Maybe k) = GetRecordMaybeSize x
  GetRecordSize (label :=>  f) = GetFieldSize f
  GetRecordSize e              = GetFieldSize e

type family
  GetRecordListSize (x :: [t]) :: Nat where
  GetRecordListSize '[] = 0
  GetRecordListSize (x ': xs) = GetRecordSize x + GetRecordListSize xs

type family
  GetRecordMaybeSize (xs :: Maybe t) :: Nat where
  GetRecordMaybeSize 'Nothing = 0
  GetRecordMaybeSize ('Just x) = GetRecordSize x

-- ** Constraints

-- | A wrapper around 'Constraint' that propagates 'TypeError'.
type ConstraintE = Either Constraint Constraint

-- | Unwrap a 'ConstraintE', this is where 'TypeError's might be /thrown/.
type family
  RunConstraintE t :: Constraint where
  RunConstraintE ('Left t) = t
  RunConstraintE ('Right t) = t

type family
  HasField (r :: rk) (l :: lk) :: Bool where
  HasField (l :=> f) l        = 'True
  HasField (f1 :>: f2) l      = HasField f1 l || HasField f2 l
  HasField f l                = 'False

type family
  HasFieldConstraint (r :: rk) (l :: lk) :: ConstraintE where
  HasFieldConstraint r l =
      If (HasField r l)
         ('Left (HasField r l ~ 'True))
         ('Right
           (TypeError ('Text "Label not found: '"
                       ':<>: 'ShowType l
                       ':<>: 'Text "' in:"
                       ':$$: 'ShowType r )))

type family
  IsFieldPostition (pos :: FieldPosition) :: Constraint where
  IsFieldPostition '(a, b) =
    If (a <=? b)
       (a <= b, KnownNat a, KnownNat b)
       (TypeError
         ('Text "Bad field position: " ':<>: 'ShowType '(a,b)
          ':$$: 'Text "First index greater than last: "
          ':<>: 'ShowType a
          ':<>: 'Text " > "
          ':<>: 'ShowType b ))

-- ** field location and access

type family
  GetFieldPosition (r :: rk) (l :: lk) :: Result FieldPosition where
  GetFieldPosition f l =
     If (HasField f l)
       ('Right (GetFieldPositionUnsafe f l))
       ('Left ('Text "Label not found. Cannot get bit range for '"
          ':<>: 'ShowType l
          ':<>: 'Text "' in:"
          ':$$: 'ShowType f ))

type family
  GetFieldPositionUnsafe (r :: rk) (l :: lk) :: FieldPosition where
  GetFieldPositionUnsafe (l :=> f)  l        = '(0, GetFieldSize f - 1)
  GetFieldPositionUnsafe (f :>: f') l        =
     If (HasField f l)
      (GetFieldPositionUnsafe f l)
      (AddToFieldPosition (GetRecordSize f) (GetFieldPositionUnsafe f' l))

type family
  AddToFieldPosition (v :: Nat) (e :: (Nat, Nat)) :: (Nat, Nat) where
  AddToFieldPosition v '(a,b) = '(a + v, b + v)

type family
  FieldPostitionToList (pos :: FieldPosition) :: [Nat] where
    FieldPostitionToList '(a, a) = '[a]
    FieldPostitionToList '(a, b) = (a ': (FieldPostitionToList '(a+1, b)))

type Align padRight a f =
    AddPadding padRight ((a - (GetRecordSize f `Rem` a)) `Rem` a) f

type family
  AddPadding (padRight :: Bool) (n :: Nat) (r :: rk) :: rk where
  AddPadding padRight 0 r = r
  AddPadding 'True n r = r :>: Field n := 0
  AddPadding 'False n r = Field n := 0 :>: r

-- ** Bit record value accessors

-- | Return the value of a single bit field as Bool
getFlag
  :: forall a (field :: fk) (first :: Nat) record p1 p2
  . ( IsFieldC field record first first
    , Bits a )
   => p1 field -> p2 record -> a -> Bool
getFlag _ _ a = testBit a pos
    where pos = fromIntegral $ natVal (Proxy :: Proxy first)

setFlag
  :: forall a field (first :: Nat) record p1 p2
  . ( IsFieldC field record first first
    , Bits a )
   => p1 field -> p2 record -> Bool -> a -> a
setFlag _ _ v a = modifyBit a pos
    where pos = fromIntegral $ natVal (Proxy :: Proxy first)
          modifyBit = if v then setBit else clearBit

getField
  :: forall a b field (first :: Nat) (last :: Nat) record pxy1 pxy2
  . ( IsFieldC field record first last
    , Integral a
    , Bits a
    , Num b)
   => pxy1 field -> pxy2 record -> a -> b
getField _ _ a = fromIntegral ((a `shiftR` posFirst) .&. bitMask)
    where
      bitMask =
        let bitCount = 1 + posLast - posFirst
            in (2 ^ bitCount) - 1
      posFirst = fromIntegral $ natVal (Proxy :: Proxy first)
      posLast = fromIntegral $ natVal (Proxy :: Proxy last)

setField
  :: forall a b field (first :: Nat) (last :: Nat) record pxy1 pxy2
  . ( IsFieldC field record first last
    , Num a
    , Bits a
    , Integral b)
   => pxy1 field -> pxy2 record -> b -> a -> a
setField _ _ v x = (x .&. bitMaskField) .|. (v' `shiftL` posFirst)
    where
      v' = bitMaskValue .&. fromIntegral v
      bitMaskField = complement (bitMaskValue `shiftL` posFirst)
      bitMaskValue =
        let bitCount = 1 + posLast - posFirst
            in (2 ^ bitCount) - 1
      posFirst = fromIntegral $ natVal (Proxy :: Proxy first)
      posLast = fromIntegral $ natVal (Proxy :: Proxy last)

type IsFieldC field record first last =
    ( RunConstraintE (record `HasFieldConstraint` field)
     , KnownNat first
     , KnownNat last
     , 'Right '(first, last) ~ (GetFieldPosition record field)
     )

-- * Field and Record PrettyType Instances

-- | Render @rec@ to a pretty, human readable form. Internally this is a wrapper
-- around 'ptShow' using 'PrettyRecord'.
showRecord
  :: forall proxy (rec :: k)
  . PrettyTypeShow (PrettyRecord rec)
  => proxy rec -> String
showRecord _ = ptShow (Proxy :: Proxy (PrettyRecord rec))

-- | A type family to pretty print @rec@ to a 'PrettyType'.
type family PrettyRecord (rec :: k) :: PrettyType
type instance PrettyRecord Word8  = PutStr "<Word8.>"
type instance PrettyRecord Int8   = PutStr "<.Int8.>"
type instance PrettyRecord Word16 = PutStr "<....Word16....>"
type instance PrettyRecord Int16  = PutStr "<....Int16.....>"
type instance PrettyRecord Word32 = PutStr "<............Word32............>"
type instance PrettyRecord Int32  = PutStr "<............Int32.............>"
type instance PrettyRecord Word64 = PutStr "<............................Word64............................>"
type instance PrettyRecord Int64  = PutStr "<............................Int64.............................>"
type instance PrettyRecord Bool = PutStr "B"
type instance PrettyRecord (x :: Maybe t) = PrettyMaybeRecord x
type instance PrettyRecord (x :: [t]) = PrettyListRecord x
type instance PrettyRecord (b :: Bool) = PutStr (If b "T" "F")
type instance PrettyRecord (FlagJust x) =
  If (x == 'Nothing)
    (PutStr "F")
    (PutStr "T")
type instance PrettyRecord (FlagNothing x) =
  If (x == 'Nothing)
    (PutStr "T")
    (PutStr "F")
type instance PrettyRecord (Field n) =
   If ( 2 <=? n )
      (PrettyOften n (PutStr "?"))
    (PutStr "<" <++> PrettyOften (n - 2) (PutStr "?") <++> PutStr ">")
type instance PrettyRecord  (l :=> r) =
    PutStr "<" <++>
    'PrettySymbol ('PrettyPadded ((GetRecordSize r) - 2)) ('PrettyPrecision ((GetRecordSize r) - 2)) l
    <++> PutStr ">"
type instance PrettyRecord (r :=  v) =
    'PrettyNat 'PrettyUnpadded ('PrettyPrecision (GetRecordSize r)) 'PrettyBit (v `Rem` (2 ^ (GetRecordSize r)))
type instance PrettyRecord (l :>: r) = PrettyRecord l <++> PrettyRecord r
type instance PrettyRecord (RecArray r n) = PrettyOften n (PrettyRecord r)
type instance PrettyRecord (Sized sizeField r) =
  ToPrettySizeField sizeField (SizeFieldValue r) <++> PutStr ":" <+> PrettyRecord r

type family ToPrettySizeField (sizeField :: SizeField) (size :: Nat) :: PrettyType where
  ToPrettySizeField 'NoSizeField s = 'PrettyEmpty
  ToPrettySizeField 'SizeField8 s = PutHex8 s
  ToPrettySizeField 'SizeField16 s = PutHex16 s
  ToPrettySizeField 'SizeField32 s = PutHex32 s

type family PrettyMaybeRecord (x :: Maybe t) :: PrettyType where
  PrettyMaybeRecord 'Nothing  = 'PrettyEmpty
  PrettyMaybeRecord ('Just x) = PrettyRecord x

type family PrettyListRecord (xs :: [t]) :: PrettyType where
  PrettyListRecord '[]  = 'PrettyEmpty
  PrettyListRecord (x ': xs) = PrettyRecord x <++> PrettyListRecord xs
