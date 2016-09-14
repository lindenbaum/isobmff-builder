{-# LANGUAGE UndecidableInstances #-}
module Data.Type.BitRecords.Core where

import Data.Int
import Data.Kind (Type, Constraint)
import Data.Kind.Extra
import Data.Proxy
import Data.Type.Pretty
import Data.Word
import GHC.TypeLits

-- * Records

-- ** Record ADT

-- | 'BitRecordField's assembly
data BitRecord where
  BitRecordMember    :: IsA (BitRecordField t) -> BitRecord
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
type OptionalRecordOf (f :: IsA (s :-> IsA BitRecord)) (x :: Maybe s) =
  (Optional (Pure 'EmptyBitRecord) f $~ x :: IsA BitRecord)

-- TODO remove??

-- ** Record PrettyPrinting

-- | Augment the pretty printed output of a 'BitRecord'
data (prettyTitle :: PrettyType) #: (r :: IsA BitRecord) :: IsA BitRecord
infixr 4 #:
type instance Eval (prettyTitle #: r)  = BitRecordAppend ('BitRecordDoc prettyTitle) (Eval r)

-- | Augment the pretty printed output of a 'BitRecord'
data (prettyTitle :: PrettyType) #$ (r :: IsA BitRecord) :: IsA BitRecord
infixr 2 #$
type instance Eval (prettyTitle #$ r) = 'BitRecordDocNested prettyTitle (Eval r)

-- ** Record composition

-- | Combine two 'BitRecord's to form a new 'BitRecord'. If the parameters are
-- not of type 'BitRecord' they will be converted.
data (:>:) (l :: IsA BitRecord) (r :: IsA BitRecord) :: IsA BitRecord
type instance Eval (l :>: r) = Eval l `BitRecordAppend` Eval r

type family BitRecordAppend (l :: BitRecord) (r :: BitRecord) :: BitRecord where
  BitRecordAppend l 'EmptyBitRecord = l
  BitRecordAppend 'EmptyBitRecord r = r
  BitRecordAppend l r = l ':>: r

-- | Append a 'BitRecord' and a 'BitRecordField'
data (:>.) :: IsA BitRecord
           -> IsA (BitRecordField t1)
           -> IsA BitRecord
infixl 6 :>.
type instance Eval (l :>. r) = BitRecordAppend (Eval l) ('BitRecordMember r)

-- | Append a 'BitRecordField' and a 'BitRecord'
data (.>:) :: IsA (BitRecordField t1)
           -> IsA BitRecord
           -> IsA BitRecord
infixr 6 .>:
type instance Eval (l .>: r) = BitRecordAppend ('BitRecordMember l) (Eval r)

-- | Append a 'BitRecordField' and a 'BitRecordField' forming a 'BitRecord' with
-- two members.
data (.>.) :: IsA (BitRecordField t1)
           -> IsA (BitRecordField t2)
           -> IsA BitRecord
infixr 6 .>.
type instance Eval (l .>. r) = BitRecordAppend ('BitRecordMember l) ('BitRecordMember r)

-- | Set a field to either a static, compile time, value or a dynamic, runtime value.
type family (:~) (field :: IsA (BitRecordField (t :: BitField (rt :: Type) (st :: k) (len :: Nat)))) (value :: IsA (FieldValue (label :: Symbol) st)) :: IsA (BitRecordField t) where
  fld :~ StaticFieldValue l v  = (l @: fld) := v
  fld :~ RuntimeFieldValue l = l @: fld
infixl 7 :~

-- | Like ':~' but for a 'Maybe' parameter. In case of 'Just' it behaves like ':~'
-- in case of 'Nothing' it return an 'EmptyBitRecord'.
type family (:~?) (fld :: IsA (BitRecordField (t :: BitField (rt :: Type) (st :: k) (len :: Nat)))) (value :: Maybe (IsA (FieldValue (label :: Symbol) st))) :: IsA BitRecord where
  fld :~? ('Just v) = RecordField (fld :~ v)
  fld :~? 'Nothing  = Pure 'EmptyBitRecord
infixl 7 :~?

-- | The field value parameter for ':~', either a static, compile time, value or
-- a dynamic, runtime value.
data FieldValue :: Symbol -> staticRep -> Type
data StaticFieldValue (label :: Symbol) :: staticRep -> IsA (FieldValue label staticRep)
data RuntimeFieldValue (label :: Symbol) :: IsA (FieldValue label staticRep)

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
  RecArrayToBitRecord r n = BitRecordAppend r (RecArrayToBitRecord r (n - 1))

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
data BitRecordField :: BitField rt st len -> Type

-- | A bit record field with a number of bits
data MkField t :: IsA (BitRecordField t)

-- **** Setting a Label

-- | A bit record field with a number of bits
data LabelF :: Symbol -> IsA (BitRecordField t) -> IsA (BitRecordField t)

-- | A field with a label assigned to it.
type
  (l :: Symbol) @:(f :: IsA (BitRecordField (t :: BitField rt (st :: stk) size)))
  = (LabelF l f :: IsA (BitRecordField t))
infixr 8 @:

-- **** Assignment

-- | A field with a value set at compile time.
data (:=) :: forall st (t :: BitField rt st size) .
            IsA (BitRecordField t)
          -> st
          -> IsA (BitRecordField t)
infixl 7 :=

-- | Types of this kind define the basic type of a 'BitRecordField'. Sure, this
-- could have been an open type, but really, how many actual useful field types
-- exist? Well, from a global perspective, uncountable infinite, but the focus
-- of this library is to blast out bits over the network, using usual Haskell
-- libraries, and hence, there is actually only very little reason to
-- differentiate types of record fields, other than what low-level library
-- function to apply and how to pretty print the field.
data BitField
     (runtimeRep :: Type)
     (staticRep :: k)
     (bitCount :: Nat)
  where
    MkFieldFlag    :: BitField Bool Bool 1
    MkFieldBits    :: forall (n :: Nat) . BitField Word64 Nat n
    MkFieldBitsXXL :: forall (n :: Nat) . BitField Integer Nat n
    MkFieldU8      :: BitField Word8 Nat 8
    MkFieldU16     :: BitField Word16 Nat 16
    MkFieldU32     :: BitField Word32 Nat 32
    MkFieldU64     :: BitField Word64 Nat 64
    MkFieldI8      :: BitField Int8  SignedNat 8
    MkFieldI16     :: BitField Int16 SignedNat 16
    MkFieldI32     :: BitField Int32 SignedNat 32
    MkFieldI64     :: BitField Int64 SignedNat 64
    -- TODO refactor this MkFieldCustom, it caused a lot of trouble!
    MkFieldCustom  :: BitField rt st n

-- *** Primitive Records and Field Types

type Flag     = MkField 'MkFieldFlag
type Field n  = MkField ('MkFieldBits :: BitField Word64 Nat n)
type FieldU8  = MkField 'MkFieldU8
type FieldU16 = MkField 'MkFieldU16
type FieldU32 = MkField 'MkFieldU32
type FieldU64 = MkField 'MkFieldU64
type FieldI8  = MkField 'MkFieldI8
type FieldI16 = MkField 'MkFieldI16
type FieldI32 = MkField 'MkFieldI32
type FieldI64 = MkField 'MkFieldI64

-- | A signed field value.
data SignedNat where
  PositiveNat :: Nat -> SignedNat
  NegativeNat :: Nat -> SignedNat

-- *** Composed Fields

-- | A Flag (1-bit) that is true if the type level maybe is 'Just'.
type family FlagJust (a :: Maybe (v :: Type)) :: IsA (BitRecordField 'MkFieldFlag) where
  FlagJust ('Just x) = Flag := 'True
  FlagJust 'Nothing  = Flag := 'False

-- | A Flag (1-bit) that is true if the type level maybe is 'Nothing'.
type family FlagNothing  (a :: Maybe (v :: Type)) :: IsA (BitRecordField 'MkFieldFlag) where
  FlagNothing ('Just x) = Flag := 'False
  FlagNothing 'Nothing  = Flag := 'True

-- | An optional field in a bit record
data MaybeField :: Maybe (IsA (BitRecordField t)) -> IsA BitRecord
type instance Eval (MaybeField ('Just  fld)) =
   BitRecordAppend ('BitRecordDoc (PutStr "Just")) ('BitRecordMember fld)
type instance Eval (MaybeField 'Nothing) =
  'BitRecordDoc (PutStr "Nothing")

-- | A 'BitRecordField' can be used as 'BitRecordMember'
data RecordField :: IsA (BitRecordField t) -> IsA BitRecord
type instance Eval (RecordField f) = 'BitRecordMember f

-- | Calculate the size as a number of bits from a 'BitRecordField'
type family BitRecordFieldSize (x :: IsA (BitRecordField t)) where
  BitRecordFieldSize (x :: IsA (BitRecordField (t :: BitField rt st size))) = size


-- * Field and Record PrettyType Instances

-- | Render @rec@ to a pretty, human readable form. Internally this is a wrapper
-- around 'ptShow' using 'PrettyRecord'.
showARecord
  :: forall proxy (rec :: IsA BitRecord) . PrettyTypeShow (PrettyRecord (Eval rec))
  => proxy rec -> String
showARecord _ = showPretty (Proxy :: Proxy (PrettyRecord (Eval rec)))

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

type instance ToPretty (f :: IsA (BitRecordField t)) = PrettyField f

type family PrettyField (f :: IsA (BitRecordField (t :: BitField (rt :: Type) (st :: Type) (size :: Nat)))) :: PrettyType where
  PrettyField (MkField t) = PrettyFieldType t
  PrettyField ((f :: IsA (BitRecordField t)) := v) =
    PrettyField f <+> PutStr ":=" <+> PrettyFieldValue t v
  PrettyField (LabelF l f) = l <:> PrettyField f

type family PrettyFieldType (t :: BitField (rt :: Type) (st :: Type) (size :: Nat)) :: PrettyType where
  PrettyFieldType ('MkFieldFlag) = PutStr "boolean"
  PrettyFieldType ('MkFieldBits :: BitField Word64 Nat (s :: Nat)) = PutStr "bits" <++> PrettyParens (PutNat s)
  PrettyFieldType ('MkFieldBitsXXL :: BitField Integer Nat (s :: Nat)) = PutStr "bits-XXL" <++> PrettyParens (PutNat s)
  PrettyFieldType ('MkFieldU64) = PutStr "U64"
  PrettyFieldType ('MkFieldU32) = PutStr "U32"
  PrettyFieldType ('MkFieldU16) = PutStr "U16"
  PrettyFieldType ('MkFieldU8) = PutStr "U8"
  PrettyFieldType ('MkFieldI64) = PutStr "I64"
  PrettyFieldType ('MkFieldI32) = PutStr "I32"
  PrettyFieldType ('MkFieldI16) = PutStr "I16"
  PrettyFieldType ('MkFieldI8) = PutStr "I8"
  PrettyFieldType ('MkFieldCustom :: BitField rt ct size) = ToPretty rt <++> PrettyParens (PutNat size)

type family PrettyFieldValue (t :: BitField (rt :: Type) (st :: Type) (size :: Nat)) (v :: st) :: PrettyType where
  PrettyFieldValue ('MkFieldFlag) 'True = PutStr "yes"
  PrettyFieldValue ('MkFieldFlag) 'False = PutStr "no"
  PrettyFieldValue ('MkFieldBits :: BitField Word64 Nat (s::Nat)) v =
    'PrettyNat 'PrettyUnpadded ('PrettyPrecision s) 'PrettyBit v  <+> PrettyParens (("hex" <:> PutHex v) <+> ("dec" <:> PutNat v))
  PrettyFieldValue ('MkFieldU8)  v = ("hex" <:> PutHex8 v) <+> PrettyParens ("dec" <:> PutNat v)
  PrettyFieldValue ('MkFieldU16) v = ("hex" <:> PutHex16 v) <+> PrettyParens ("dec" <:> PutNat v)
  PrettyFieldValue ('MkFieldU32) v = ("hex" <:> PutHex32 v) <+> PrettyParens ("dec" <:> PutNat v)
  PrettyFieldValue ('MkFieldU64) v = ("hex" <:> PutHex64 v) <+> PrettyParens ("dec" <:> PutNat v)
  PrettyFieldValue ('MkFieldI8)  ('PositiveNat v) = ("hex" <:> (PutStr "+" <++> PutHex8 v)) <+> PrettyParens ("dec"  <:> (PutStr "+" <++> PutNat v))
  PrettyFieldValue ('MkFieldI16) ('PositiveNat v) = ("hex" <:> (PutStr "+" <++> PutHex16 v)) <+> PrettyParens ("dec" <:> (PutStr "+" <++> PutNat v))
  PrettyFieldValue ('MkFieldI32) ('PositiveNat v) = ("hex" <:> (PutStr "+" <++> PutHex32 v)) <+> PrettyParens ("dec" <:> (PutStr "+" <++> PutNat v))
  PrettyFieldValue ('MkFieldI64) ('PositiveNat v) = ("hex" <:> (PutStr "+" <++> PutHex64 v)) <+> PrettyParens ("dec" <:> (PutStr "+" <++> PutNat v))
  PrettyFieldValue ('MkFieldI8)  ('NegativeNat v) = ("hex" <:> (PutStr "-" <++> PutHex8 v)) <+> PrettyParens ("dec"  <:> (PutStr "-" <++> PutNat v))
  PrettyFieldValue ('MkFieldI16) ('NegativeNat v) = ("hex" <:> (PutStr "-" <++> PutHex16 v)) <+> PrettyParens ("dec" <:> (PutStr "-" <++> PutNat v))
  PrettyFieldValue ('MkFieldI32) ('NegativeNat v) = ("hex" <:> (PutStr "-" <++> PutHex32 v)) <+> PrettyParens ("dec" <:> (PutStr "-" <++> PutNat v))
  PrettyFieldValue ('MkFieldI64) ('NegativeNat v) = ("hex" <:> (PutStr "-" <++> PutHex64 v)) <+> PrettyParens ("dec" <:> (PutStr "-" <++> PutNat v))
  PrettyFieldValue ('MkFieldCustom :: BitField rt ct size) v = PrettyCustomFieldValue rt ct size v

type family PrettyCustomFieldValue (rt :: Type) (st :: Type) (size :: Nat) (v :: st) :: PrettyType
