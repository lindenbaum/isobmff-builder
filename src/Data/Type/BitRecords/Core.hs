{-# LANGUAGE UndecidableInstances #-}
module Data.Type.BitRecords.Core where

import Data.Int
import Data.Kind (Type, Constraint)
import Data.Proxy
import Data.Type.BitRecords.Arithmetic
import Data.Tagged
import Data.Type.Pretty
import Data.Word
import GHC.TypeLits


-- * Records

-- ** Record ADT

-- | 'BitRecordField's assembly
data BitRecord where
  BitRecordMember    :: BitRecordField -> BitRecord
  AppendedBitRecords :: BitRecord      -> BitRecord -> BitRecord
  ReplacePretty      :: PrettyType     -> BitRecord -> BitRecord
  EmptyBitRecord     :: BitRecord
  -- TODO  MissingBitRecord          :: ErrorMessage     -> BitRecord

-- | Type class of types that can be represented as a 'BitRecord'
type family ToBitRecord (t :: k) :: BitRecord
type instance ToBitRecord (x :: BitRecord) = x

-- | A conditional 'BitRecord'
type family WhenR (b :: Bool) (x :: k) :: BitRecord where
  WhenR 'False r = 'EmptyBitRecord
  WhenR 'True r  = ToBitRecord r

-- *** Basic Accessor

-- | Extract the size in as a number of bits from a 'BitRecord'
type family BitRecordSize (x :: BitRecord) :: Nat where
  BitRecordSize ('BitRecordMember f)      = BitRecordFieldSize f
  BitRecordSize ('AppendedBitRecords l r) = BitRecordSize l + BitRecordSize r
  BitRecordSize ('ReplacePretty a r)      = BitRecordSize r
  BitRecordSize 'EmptyBitRecord           = 0

-- | The total number of members in a record.
type family BitRecordMemberCount (b :: BitRecord) :: Nat where
  BitRecordMemberCount ('BitRecordMember f)      = 1
  BitRecordMemberCount ('AppendedBitRecords l r) = BitRecordMemberCount l + BitRecordMemberCount r
  BitRecordMemberCount ('ReplacePretty a r)      = BitRecordMemberCount r
  BitRecordMemberCount 'EmptyBitRecord           = 0

-- | Return the size of the record.
getRecordSizeFromProxy
  :: forall px (rec :: BitRecord) . KnownNat (BitRecordSize rec) => px rec -> Integer
getRecordSizeFromProxy _ = natVal (Proxy :: Proxy (BitRecordSize rec))

-- ** Record composition

-- | Combine two 'BitRecord's to form a new 'BitRecord'. If the parameters are
-- not of type 'BitRecord' they will be converted.
type family (:>:) (l :: j) (r :: k) :: BitRecord where
 (:>:) 'EmptyBitRecord r               = ToBitRecord r
 (:>:) l 'EmptyBitRecord               = ToBitRecord l
 (:>:) (l :: BitRecord) (r :: BitRecord) = 'AppendedBitRecords l r
 (:>:) l r                             = ToBitRecord l :>: ToBitRecord r
infixl 3 :>:

-- *** Record Arrays and Repitition

-- | An array of records with a fixed number of elements, NOTE: this type is
-- actually not really necessary since 'ReplicateRecord' exists, but this allows
-- to have a different 'showRecord' output.
data RecArray :: k -> Nat -> Type

type r ^^ n = RecArray r n
infixr 8 ^^

type instance ToBitRecord (RecArray (r :: k) n) = RecArrayToBitRecord r n

-- | Repeat a bit record @n@ times.
type family RecArrayToBitRecord (rec :: k) (n :: Nat) :: BitRecord where
  RecArrayToBitRecord r 0 = 'EmptyBitRecord
  RecArrayToBitRecord r 1 = ToBitRecord r
  RecArrayToBitRecord r n = 'AppendedBitRecords (ToBitRecord r) (RecArrayToBitRecord r (n - 1))

-- *** Lists of Records

-- | Let type level lists also be recrods
type instance ToBitRecord (xs :: [k]) = ListToBitRecord xs

type family ListToBitRecord (xs :: [k]) :: BitRecord where
  ListToBitRecord '[] = 'EmptyBitRecord
  ListToBitRecord (x ': xs )  = 'AppendedBitRecords (ToBitRecord x) (ListToBitRecord xs)

-- *** Maybe Record

type instance ToBitRecord ('Nothing :: Maybe k) = 'EmptyBitRecord
type instance ToBitRecord ('Just r :: Maybe k) = ToBitRecord r

-- ** Field ADT

-- | A family of bit fields.
--
-- A bit field always has a size, i.e. the number of bits it uses, as well as a
-- term level value type and a type level value type. It also has an optional
-- label, and an optional value assigned to it.
data BitRecordField where
  -- | A field with a value set at compile time.
  AssignF :: value     -> BitRecordField -> BitRecordField
  -- | A bit record field with a number of bits
  MkField :: demoteRep -> Nat -> BitRecordField

-- | A 'BitRecordField' can be used as 'BitRecordMember'
type instance ToBitRecord (a :: BitRecordField) = 'BitRecordMember a

-- | Type class of types that can be represented as a 'BitRecordField'
type family ToBitRecordField (t :: k) :: BitRecordField
type instance ToBitRecordField (x :: BitRecordField) = x

-- | Extract the size in as a number of bits from a 'BitRecordField'
type family BitRecordFieldSize (x :: BitRecordField) where
  BitRecordFieldSize ('MkField dr size) = size
  BitRecordFieldSize ('AssignF l f)     = BitRecordFieldSize f

-- | Extract the value level assignment type a 'BitRecordField'
type family BitFieldDemoteRep (x :: BitRecordField) where
  BitFieldDemoteRep ('MkField dr size) = dr
  BitFieldDemoteRep ('AssignF l f)     = BitFieldDemoteRep f

-- *** Field Constructor

-- **** Setting a Label

-- | A field with a label assigned to it.
--
-- The left argument is converted to a 'BitRecordField' via 'ToBitRecordField'
-- in necessary. The right argument must match the parameter of the 'Maybe' of
-- the corresponding field in 'BitRecordField'.
type family (:=>) (l :: Symbol) (b :: BitRecordField) :: BitRecordField where
  (:=>) label ('MkField dr size) = 'MkField (Tagged label dr) size
  (:=>) label ('AssignF v f) = 'AssignF v (label :=> f)
infixr 5 :=>

-- **** Assignment

-- | A field with a (type-level-) value assigned to.
--
-- The left argument is converted to a 'BitRecordField' via 'ToBitRecordField'
-- in necessary. The right argument must match the parameter of the 'Maybe' of
-- the corresponding field in 'BitRecordField'.
type family (:=) (b :: BitRecordField) (v :: k) :: BitRecordField where
  (:=) ('MkField demoteRep size) v = 'AssignF v ('MkField demoteRep size)
  (:=) o v = TypeError ('Text "Cannot assign (type-level-) value "
                         ':<>: 'ShowType v
                         ':<>: 'Text " to field: "
                         ':<>: 'ShowType o)
infixr 6 :=

-- *** Primitive Records and Field Types

-- | A single bit (boolean) field
type Flag = 'MkField Bool 1

-- | A single bit (Boolean) field
type instance ToBitRecordField Bool = Flag

-- | A promoted Bool field
type instance ToBitRecordField (x :: Bool) = Flag := x

-- | A single bit (Boolean) record
type instance ToBitRecord Bool = ToBitRecord Flag

-- | A promoted Bool record
type instance ToBitRecord (x :: Bool) = 'BitRecordMember (ToBitRecordField x)

-- | Define a field of bits with a size and 'Word64' as default demote rep.
type Field n = 'MkField Word64 n

type FieldU8 = 'MkField Word8 8

type FieldU16 = 'MkField Word16 16

type FieldU32 =
  'MkField Word32 32

type FieldU64 =
  'MkField Word64 64

-- | A signed field value.
data SignedNat where
  PositiveNat :: Nat -> SignedNat
  NegativeNat :: Nat -> SignedNat

type FieldI8 =
  'MkField Int8 8

type FieldI16 =
  'MkField Int16 16

type FieldI32 =
  'MkField Int32 32

type FieldI64 =
  'MkField Int64 64

-- *** Composed Fields

-- | A Flag (1-bit) that is true if the type level maybe is 'Just'.
type family FlagJust a :: BitRecordField where
  FlagJust ('Just x) = Flag := 'True
  FlagJust 'Nothing  = Flag := 'False

-- | A Flag (1-bit) that is true if the type level maybe is 'Nothing'.
type family FlagNothing a :: BitRecordField where
  FlagNothing ('Just x) = Flag := 'False
  FlagNothing 'Nothing  = Flag := 'True

-- | A field that renders to the length of a 'SizedString' using the given
-- word type for the size.
data ToStringLength :: Type -> Type -> Type -- TODO move

-- ** Auto-Padding

type Align padRight a f =
    AddPadding padRight ((a - (BitRecordSize f `Rem` a)) `Rem` a) f

type family
  AddPadding (padRight :: Bool) (n :: Nat) (r :: rk) :: rk where
  AddPadding padRight 0 r = r
  AddPadding 'True n r = r :>: Field n := 0
  AddPadding 'False n r = Field n := 0 :>: r

-- * Field and Record PrettyType Instances

-- | Render @rec@ to a pretty, human readable form. Internally this is a wrapper
-- around 'ptShow' using 'PrettyRecord'.
showRecord
  :: forall proxy (rec :: BitRecord) . PrettyTypeShow (PrettyRecord rec)
  => proxy rec -> String
showRecord _ = showPretty (Proxy :: Proxy (PrettyRecord rec))

type family PrettyRecord (rec :: BitRecord) :: PrettyType where
   PrettyRecord ('BitRecordMember m) = "Field" <:$$--> PrettyField m
   PrettyRecord 'EmptyBitRecord = 'PrettyEmpty
   PrettyRecord ('AppendedBitRecords l r) = PrettyRecord l <$$> PrettyRecord r
   PrettyRecord ('ReplacePretty p r) = p

type family PrettyField (f :: BitRecordField) :: PrettyType where
  PrettyField ('AssignF v f) =
    PrettyField f <$$> "Static Value" <:> ToPretty v
  PrettyField ('MkField demRep size) =
       "Demote Rep" <:> ToPretty demRep <$$>
       "Bits" <:> PutNat size

-- * Constraint Utilities

-- | A wrapper around 'Constraint' that propagates 'TypeError'.
type ConstraintE = Either Constraint Constraint

-- | Unwrap a 'ConstraintE', this is where 'TypeError's might be /thrown/.
type family
  RunConstraintE t :: Constraint where
  RunConstraintE ('Left t) = t
  RunConstraintE ('Right t) = t

-- * Support for /tables/ where entries are mapped to 'Nat's

-- | Return the numeric /index/ of an entry in a table. This emulates 'fromEnum' a bit.
type family FromEnum enum (entry :: enum) :: Nat
