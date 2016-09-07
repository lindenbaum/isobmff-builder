{-# LANGUAGE UndecidableInstances #-}
module Data.Type.BitRecords.Core where

import Data.Int
import Data.Kind (Type, Constraint)
import Data.Kind.Extra
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
  (:>:)              :: BitRecord      -> BitRecord -> BitRecord
  BitRecordDoc       :: PrettyType     -> BitRecord
  BitRecordDocNested :: PrettyType     -> BitRecord -> BitRecord
  EmptyBitRecord     :: BitRecord
  -- TODO  MissingBitRecord          :: ErrorMessage     -> BitRecord

-- | A conditional 'BitRecord'
type family WhenR (b :: Bool) (x :: BitRecord) :: BitRecord where
  WhenR 'False r = 'EmptyBitRecord
  WhenR 'True r  = r

-- | /Smart/ constructor for bit records.
--
-- A constructor that takes any type to a 'BitRecord' that has one of the
-- following kinds:
--
-- 1. 'IsA BitRecord'
-- 2. 'IsA BitRecordField'
-- 3. 'IsA (BitRecordOf t)'
-- 4. 'IsA k', where @x '-->' BitRecord@ is 'Eval'ed
--
type family ToBitRecord (x :: IsA k) :: BitRecord where
  ToBitRecord (x :: IsA BitRecord) = Eval x
  ToBitRecord (x :: IsA BitRecordField) = 'BitRecordMember (Eval x)
  ToBitRecord (x :: IsA (BitRecordOf t)) = Eval x
  ToBitRecord (x :: IsA k) = x -->| BitRecord

-- *** Basic Accessor

-- | Extract the size in as a number of bits from a 'BitRecord'
type family BitRecordSize (x :: BitRecord) :: Nat where
  BitRecordSize ('BitRecordMember f)      = BitRecordFieldSize f
  BitRecordSize (l ':>: r)                = BitRecordSize l + BitRecordSize r
  BitRecordSize ('BitRecordDoc d)         = 0
  BitRecordSize ('BitRecordDocNested d r) = BitRecordSize r
  BitRecordSize 'EmptyBitRecord           = 0

-- | The total number of members in a record.
type family BitRecordMemberCount (b :: BitRecord) :: Nat where
  BitRecordMemberCount ('BitRecordMember f)      = 1
  BitRecordMemberCount (l ':>: r)                = BitRecordMemberCount l + BitRecordMemberCount r
  BitRecordMemberCount ('BitRecordDoc r)         = 0
  BitRecordMemberCount ('BitRecordDocNested d r) = BitRecordMemberCount r
  BitRecordMemberCount 'EmptyBitRecord           = 0

-- | Return the size of the record.
getRecordSizeFromProxy
  :: forall px (rec :: BitRecord) . KnownNat (BitRecordSize rec) => px rec -> Integer
getRecordSizeFromProxy _ = natVal (Proxy :: Proxy (BitRecordSize rec))

-- ** Type-Tagged Record Creation

-- | The kind of 'BitRecord' constructors for some type.
--
-- It's just a container to carry a type/kind around, such that  type functions
-- can restrict the set of applicable parameters. I think of it as @/newkind/@
-- somehow, but maybe it is better understood as /phantom kind/. The
-- 'BitRecordOf' is just an indicator of the type of types (hence it is
-- actually a kind of types), it never carries  any actual type value.
data BitRecordOf t where
  MkBitRecord :: BitRecord -> BitRecordOf f

type instance Eval ('MkBitRecord br ~~> BitRecord) = br

-- | Either use the value from @Just@ or return a 'EmptyBitRecord' value(types(kinds))
data OptionalRecordOf :: Maybe (IsA (BitRecordOf t)) -> IsA (BitRecordOf t)
type instance Eval (OptionalRecordOf ('Just t)) = Eval t
type instance Eval (OptionalRecordOf 'Nothing) = 'MkBitRecord 'EmptyBitRecord

-- | A setter for 'f', that assigns a value.
--
-- An alias for 'SetWith' using the 'StdSetter' 'Assign'.
type SetTo f v = SetWith f (OverwriteWith v)

-- | Alternative version of 'SetTo'
type SetToAlt f v = SetWith f (AltSetter (OverwriteWith v))

-- | A setter for 'f', that creates a named parameter for runtime
-- values to set the field.
--
-- An alias for 'SetWith' using the 'StdSetter' 'Assign'.
type Defer label f = SetWith f (NamedRuntimeParameter label)

-- | Alternative version of 'Defer'
type DeferAlt label f = SetWith f (AltSetter (NamedRuntimeParameter label))

-- | Set something to a value in a specific way.
--
-- The way how a field is assigned to a value is controlled through the 'Eval'
-- instances, that can vary depending on @f@ and @setter v@.
data SetWith :: IsAn f -> IsA Setter -> IsAn f

-- | Abstract kind of 'SetWith' setter parameter
data Setter

-- | Assign to a value known at compile-time.
data OverwriteWith v :: IsA Setter

-- | Assign a value obtained from a named parameter, e.g. with a value
-- obtained at runtime.
data NamedRuntimeParameter :: Symbol -> IsA Setter

-- | Assign a value obtained from a named parameter, e.g. with a value
-- obtained at runtime.
data AltSetter :: IsA Setter -> IsA Setter

-- ** Record PrettyPrinting

-- | Augment the pretty printed output of a 'BitRecord'
type prettyTitle #: r = ('BitRecordDoc prettyTitle) ':>: r
infixr 2 #:
-- | Augment the pretty printed output of a 'BitRecord'
type prettyTitle #$ r = 'BitRecordDocNested prettyTitle r
infixr 1 #$

-- ** Record composition

-- | Combine two 'BitRecord's to form a new 'BitRecord'. If the parameters are
-- not of type 'BitRecord' they will be converted.
data (:>:) (l :: IsA BitRecord) (r :: IsA BitRecord) :: IsA BitRecord

type instance Eval (l :>: r) = Eval l ':>: Eval r
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
  RecArrayToBitRecord r n = (ToBitRecord r) ':>: (RecArrayToBitRecord r (n - 1))

-- *** Lists of Records

-- | Let type level lists also be recrods
type instance ToBitRecord (xs :: [k]) = ListToBitRecord xs

type family ListToBitRecord (xs :: [k]) :: BitRecord where
  ListToBitRecord '[] = 'EmptyBitRecord
  ListToBitRecord (x ': xs )  =  (ToBitRecord x) ':>: (ListToBitRecord xs)

-- *** Maybe Record

-- | Either use the value from @Just@ or return a 'EmptyBitRecord' value(types(kinds))
data OptionalRecord :: Maybe (IsA BitRecord) -> IsA BitRecord
type instance Eval (OptionalRecord ('Just t)) = Eval t
type instance Eval (OptionalRecord 'Nothing) = 'EmptyBitRecord

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
type family (:=>) (l :: Symbol) (b :: BitRecordField) :: BitRecordField where
  (:=>) label ('MkField dr size) = 'MkField (Tagged label dr) size
  (:=>) label ('AssignF v f) = 'AssignF v (label :=> f)
infixr 5 :=>

-- **** Assignment

-- | A field with a (type-level-) value assigned to.
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

-- | An optional field
data MaybeField :: BitRecordField -> Maybe v -> IsA BitRecord
type instance Eval (MaybeField f ('Just  v)) =
  PutStr "Just" #: 'BitRecordMember (f := v)
type instance Eval (MaybeField f 'Nothing) =
  PutStr "Nothing" #: 'EmptyBitRecord

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

type instance ToPretty (rec :: BitRecord) = PrettyRecord rec

type family PrettyRecord (rec :: BitRecord) :: PrettyType where
   PrettyRecord ('BitRecordMember m) = PrettyField m
   PrettyRecord ' EmptyBitRecord = 'PrettyNewline
   PrettyRecord (l ':>: r) = PrettyRecord l <$$> PrettyRecord r
   PrettyRecord ('BitRecordDoc p) = p
   PrettyRecord ('BitRecordDocNested p r) = p <$$--> PrettyRecord r

type instance ToPretty (f :: BitRecordField) = PrettyField f

type family PrettyField (f :: BitRecordField) :: PrettyType where
  PrettyField ('MkField (Tagged (label :: Symbol) demRep) size) =
    label <:> PrettyField ('MkField demRep size)
  PrettyField ('MkField demRep size) =  PutStr "field" <++> PrettyParens (PutNat size)
  PrettyField ('AssignF v ('MkField demRep size)) =
    PrettyField ('MkField demRep size) <+> PutStr ":=" <+> PrettyFieldValue v demRep size
  PrettyField ('AssignF v f) = PrettyField f

type family PrettyFieldValue v demRep (size :: Nat) where
  PrettyFieldValue (v :: Nat) Word64 size =
    'PrettyNat 'PrettyUnpadded ('PrettyPrecision size) 'PrettyBit v
    <+> PrettyParens (("hex" <:> PutHex v) <+> ("dec" <:> PutNat v))
  PrettyFieldValue v d s = ToPretty v -- TODO remove this and add all the missing peices

-- * Constraint Utilities

-- | A wrapper around 'Constraint' that propagates 'TypeError'.
type ConstraintE = Either Constraint Constraint

-- | Unwrap a 'ConstraintE', this is where 'TypeError's might be /thrown/.
type family
  RunConstraintE t :: Constraint where
  RunConstraintE ('Left t) = t
  RunConstraintE ('Right t) = t
