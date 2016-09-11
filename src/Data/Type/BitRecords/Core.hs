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
infixl 3 :>:

-- | A conditional 'BitRecord'
type family WhenR (b :: Bool) (x :: BitRecord) :: BitRecord where
  WhenR 'False r = 'EmptyBitRecord
  WhenR 'True r  = r

-- *** Basic Accessor

-- | Eval the size in as a number of bits from a 'BitRecord'
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

-- | Either use the value from @Just@ or return a 'EmptyBitRecord' value(types(kinds))
type OptionalRecordOf f s =
  (Optional (Pure 'EmptyBitRecord) f $~ s :: IsA BitRecord)

-- TODO remove??

-- ** Record PrettyPrinting

-- | Augment the pretty printed output of a 'BitRecord'
data (prettyTitle :: PrettyType) #: (r :: IsA BitRecord) :: IsA BitRecord
infixr 4 #:
type instance Eval (prettyTitle #: r)  = ('BitRecordDoc prettyTitle) ':>: Eval r

-- | Augment the pretty printed output of a 'BitRecord'
data (prettyTitle :: PrettyType) #$ (r :: IsA BitRecord) :: IsA BitRecord
infixr 2 #$
type instance Eval (prettyTitle #$ r) = 'BitRecordDocNested prettyTitle (Eval r)

-- ** Record composition

-- | Combine two 'BitRecord's to form a new 'BitRecord'. If the parameters are
-- not of type 'BitRecord' they will be converted.
data (:>:) (l :: IsA BitRecord) (r :: IsA BitRecord) :: IsA BitRecord
type instance Eval (l :>: r) = Eval l ':>: Eval r

-- | Append a 'BitRecord' and a 'BitRecordField'
data (:>.) (l :: IsA BitRecord) (r :: IsA BitRecordField) :: IsA BitRecord
infixl 6 :>.
type instance Eval (l :>. r) = Eval l ':>: 'BitRecordMember (Eval r)

-- | Append a 'BitRecordField' and a 'BitRecord'
data (.>:) (l :: IsA BitRecordField) (r :: IsA BitRecord) :: IsA BitRecord
infixr 6 .>:
type instance Eval (l .>: r) = 'BitRecordMember (Eval l) ':>: Eval r

-- | Append a 'BitRecordField' and a 'BitRecordField' forming a 'BitRecord' with
-- two members.
data (.>.) (l :: IsA BitRecordField) (r :: IsA BitRecordField) :: IsA BitRecord
infixr 6 .>.
type instance Eval (l .>. r) = 'BitRecordMember (Eval l) ':>: 'BitRecordMember (Eval r)

-- | Set a field to either a static, compile time, value or a dynamic, runtime value.
data (:~) :: forall (k :: Type) . IsA BitRecordField -> IsA (FieldValue k) -> IsA BitRecordField
infixl 7 :~
type instance Eval (fld :~ StaticFieldValue v)  = Eval (fld := v)
type instance Eval (fld :~ RuntimeFieldValue l) = Eval (l   @: fld)

-- | Like ':~' but for a 'Maybe' parameter. In case of 'Just' it behaves like ':~'
-- in case of 'Nothing' it return an 'EmptyBitRecord'.
data (:~?) :: forall (k :: Type) . IsA BitRecordField -> Maybe (IsA (FieldValue k)) -> IsA BitRecord
infixl 7 :~?
type instance Eval (fld :~? ('Just v)) = 'BitRecordMember (Eval (fld :~ v))
type instance Eval (fld :~? 'Nothing) = 'EmptyBitRecord

-- | The field value parameter for ':~', either a static, compile time, value or
-- a dynamic, runtime value.
data FieldValue demoteRep
data StaticFieldValue  :: value  -> IsA (FieldValue demoteRep)
data RuntimeFieldValue :: Symbol -> IsA (FieldValue demoteRep)

-- *** Record Arrays and Repitition

-- | An array of records with a fixed number of elements, NOTE: this type is
-- actually not really necessary since 'ReplicateRecord' exists, but this allows
-- to have a different 'showRecord' output.
data RecArray :: IsA BitRecord -> Nat -> IsA BitRecord

type r ^^ n = RecArray r n
infixl 5 ^^

type instance Eval (RecArray (r :: IsA BitRecord) n ) = RecArrayToBitRecord (Eval r) n

-- | Repeat a bit record @n@ times.
type family RecArrayToBitRecord (r :: BitRecord) (n :: Nat) :: BitRecord where
  RecArrayToBitRecord r 0 = 'EmptyBitRecord
  RecArrayToBitRecord r 1 = r
  RecArrayToBitRecord r n = r ':>: RecArrayToBitRecord r (n - 1)

-- *** Lists of Records

-- | Let type level lists also be records
type family
    BitRecordOfList
      (f :: IsA ((foo :: Type) :-> IsA BitRecord))
      (xs :: [foo])
      :: IsA BitRecord
  where
    BitRecordOfList f xs = FoldMap (Fun2 (:>:)) (Pure 'EmptyBitRecord) f xs

-- *** Maybe Record

-- | Either use the value from @Just@ or return a 'EmptyBitRecord' value(types(kinds))
data OptionalRecord :: Maybe (IsA BitRecord) -> IsA BitRecord
type instance Eval (OptionalRecord ('Just t)) = Eval t
type instance Eval (OptionalRecord 'Nothing)  = 'EmptyBitRecord

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
data RecordField :: IsA BitRecordField -> IsA BitRecord
type instance Eval (RecordField f) = 'BitRecordMember (Eval f)

-- | Eval the size in as a number of bits from a 'BitRecordField'
type family BitRecordFieldSize (x :: BitRecordField) where
  BitRecordFieldSize ('MkField dr size) = size
  BitRecordFieldSize ('AssignF l f)     = BitRecordFieldSize f

-- | Eval the value level assignment type a 'BitRecordField'
type family BitFieldDemoteRep (x :: BitRecordField) where
  BitFieldDemoteRep ('MkField dr size) = dr
  BitFieldDemoteRep ('AssignF l f)     = BitFieldDemoteRep f

-- *** Field Constructor

-- **** Setting a Label

-- | A field with a label assigned to it.
data (@:) (l :: Symbol) (b :: IsA BitRecordField) :: IsA BitRecordField
infixr 8 @:
type instance Eval (l @: f) = TagBitRecodField l (Eval f)
type family TagBitRecodField (l :: Symbol) (b :: BitRecordField) :: BitRecordField where
  TagBitRecodField l ('MkField dr size) = 'MkField (Tagged l dr) size
  TagBitRecodField l ('AssignF v f)     = 'AssignF (Tagged l v) (TagBitRecodField l f)

-- | Assign a label to a field.
data (label :: Symbol) @:: (fld :: IsAn (IsA BitRecordField)) :: IsAn (IsA BitRecordField)
infixr 8 @::
type instance Eval (label @:: fld) =
  (label @: (Eval fld))

type (@:::) label f = Pure ((@:) label) f

-- **** Assignment

-- | A field with a (type-level-) value assigned to.
data (:=) (b :: IsA BitRecordField) (v :: k) :: IsA BitRecordField
infixl 7 :=

type instance Eval ((b :: IsA BitRecordField) := (v :: k)) = AssignFChecked (Eval b) v

type family AssignFChecked (b :: BitRecordField) (v :: k) :: BitRecordField where
  AssignFChecked ('MkField demoteRep size) v = 'AssignF v ('MkField demoteRep size)
  AssignFChecked o v = TypeError ('Text "Cannot assign (type-level-) value "
                         ':<>: 'ShowType v
                         ':<>: 'Text " to field, that is already assigned: "
                         ':<>: 'ShowType o)

-- | Assign a value (type-level) to a field assignment.
data (fld :: IsAn (IsA BitRecordField)) ::= (value :: vk) :: IsAn (IsA BitRecordField)
type instance Eval (fld ::= v) = (Eval fld := v)
infixr 7 ::=

-- *** Primitive Records and Field Types

-- | A single bit (boolean) field
data Flag :: IsA BitRecordField
type instance Eval Flag = 'MkField Bool 1

-- | Define a field of bits with a size and 'Word64' as default demote rep.
data Field n :: IsA BitRecordField
type instance Eval (Field n) = 'MkField Word64 n

data FieldU8 :: IsA BitRecordField
type instance Eval (FieldU8) = 'MkField Word8 8

data FieldU16 :: IsA BitRecordField
type instance Eval (FieldU16) = 'MkField Word16 16

data FieldU32 :: IsA BitRecordField
type instance Eval (FieldU32) = 'MkField Word32 32

data FieldU64 :: IsA BitRecordField
type instance Eval (FieldU64) =
  'MkField Word64 64

-- | A signed field value.
data SignedNat where
  PositiveNat :: Nat -> SignedNat
  NegativeNat :: Nat -> SignedNat

data FieldI8 :: IsA BitRecordField
type instance Eval (FieldI8) =
  'MkField Int8 8

data FieldI16 :: IsA BitRecordField
type instance Eval (FieldI16) =
  'MkField Int16 16

data FieldI32 :: IsA BitRecordField
type instance Eval (FieldI32) =
  'MkField Int32 32

data FieldI64 :: IsA BitRecordField
type instance Eval (FieldI64) =
  'MkField Int64 64

-- *** Composed Fields

-- | A Flag (1-bit) that is true if the type level maybe is 'Just'.
data FlagJust (a :: Maybe v) :: IsA BitRecordField
type instance Eval (FlagJust ('Just x)) = Eval (Flag := 'True)
type instance Eval (FlagJust 'Nothing)  = Eval (Flag := 'False)

-- | A Flag (1-bit) that is true if the type level maybe is 'Nothing'.
data FlagNothing (a :: Maybe v) :: IsA BitRecordField
type instance Eval (FlagNothing ('Just x)) = Eval (Flag := 'False)
type instance Eval (FlagNothing 'Nothing)  = Eval (Flag := 'True)

-- | A field that renders to the length of a 'SizedString' using the given
-- word type for the size.
data ToStringLength :: Type -> Type -> Type -- TODO move

-- | An optional field in a bit record
data MaybeField :: Maybe (IsA BitRecordField) -> IsA BitRecord
type instance Eval (MaybeField ('Just  fld)) =
  'BitRecordDoc (PutStr "Just") ':>: 'BitRecordMember (Eval fld)
type instance Eval (MaybeField 'Nothing) =
  'BitRecordDoc (PutStr "Nothing")

-- ** Auto-Padding

type Align padRight a f =
    AddPadding padRight ((a - (BitRecordSize f `Rem` a)) `Rem` a) f

type family
  AddPadding (padRight :: Bool) (n :: Nat) (r :: BitRecord) :: BitRecord where
  AddPadding padRight 0 r = r
  AddPadding 'True n r  = r ':>: 'BitRecordMember ('AssignF 0 ('MkField Word64 n))
  AddPadding 'False n r =  'BitRecordMember ('AssignF 0 ('MkField Word64 n)) ':>: r

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

type family PrettyFieldValue v (demRep :: Type) (size :: Nat) where
  -- TODO
  -- PrettyFieldValue ('False :: Bool) Bool 1 = PutStr "False"
  -- PrettyFieldValue ('True :: Bool) Bool 1 = PutStr "True"

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











-- -- | A setter for 'f', that assigns a value.
-- --
-- -- An alias for 'SetWith' using the 'StdSetter' 'Assign'.
-- type SetTo f v = SetWith f (OverwriteWith v)

-- -- | Alternative version of 'SetTo'
-- type SetToAlt f v = SetWith f (AltSetter (OverwriteWith v))

-- -- | A setter for 'f', that creates a named parameter for runtime
-- -- values to set the field.
-- --
-- -- An alias for 'SetWith' using the 'StdSetter' 'Assign'.
-- type Defer label f = SetWith f (NamedRuntimeParameter label)

-- -- | Alternative version of 'Defer'
-- type DeferAlt label f = SetWith f (AltSetter (NamedRuntimeParameter label))

-- -- | Set something to a value in a specific way.
-- --
-- -- The way how a field is assigned to a value is controlled through the 'Eval'
-- -- instances, that can vary depending on @f@ and @setter v@.
-- data SetWith :: IsAn f -> IsA Setter -> IsAn f

-- -- | Abstract kind of 'SetWith' setter parameter
-- data Setter

-- -- | Assign to a value known at compile-time.
-- data OverwriteWith v :: IsA Setter

-- -- | Assign a value obtained from a named parameter, e.g. with a value
-- -- obtained at runtime.
-- data NamedRuntimeParameter :: Symbol -> IsA Setter

-- -- | Assign a value obtained from a named parameter, e.g. with a value
-- -- obtained at runtime.
-- data AltSetter :: IsA Setter -> IsA Setter
