{-# LANGUAGE UndecidableInstances #-}
module Data.Type.BitRecords.Core where

import Data.Int
import Data.Kind (Type)
import Data.Kind.Extra
import Data.Proxy
import Data.Type.Pretty
import Data.Word
import Data.Bits
import GHC.TypeLits
import Text.Printf

-- * Bit-Records

-- ** Bit-Record Type

-- | 'BitRecordField's assembly
data BitRecord where
  EmptyBitRecord     :: BitRecord
  BitRecordMember    :: IsA (BitRecordField t) -> BitRecord
  BitRecordAppend    :: BitRecord -> BitRecord -> BitRecord
  -- TODO  MissingBitRecord          :: ErrorMessage     -> BitRecord

-- | A conditional 'BitRecord'
type family WhenR (b :: Bool) (x :: BitRecord) :: BitRecord where
  WhenR 'False r = 'EmptyBitRecord
  WhenR 'True r  = r

-- *** Basic Accessor

-- | Get the number of bits in a 'BitRecord'
type family BitRecordSize (x :: BitRecord) :: Nat where
  BitRecordSize 'EmptyBitRecord           = 0
  BitRecordSize ('BitRecordMember f)      = BitRecordFieldSize f
  BitRecordSize ('BitRecordAppend l r)    = BitRecordSize l + BitRecordSize r

-- | Get the total number of members in a record.
type family BitRecordMemberCount (b :: BitRecord) :: Nat where
  BitRecordMemberCount 'EmptyBitRecord           = 0
  BitRecordMemberCount ('BitRecordMember f)      = 1
  BitRecordMemberCount ('BitRecordAppend l r)    = BitRecordMemberCount l + BitRecordMemberCount r

-- | Get the size of the record.
getRecordSizeFromProxy
  :: forall px (rec :: BitRecord) . KnownNat (BitRecordSize rec) => px rec -> Integer
getRecordSizeFromProxy _ = natVal (Proxy :: Proxy (BitRecordSize rec))

-- | Either use the value from @Just@ or return a 'EmptyBitRecord' value(types(kinds))
type OptionalRecordOf (f :: IsA (s :-> IsA BitRecord)) (x :: Maybe s) =
  (Optional (Pure 'EmptyBitRecord) f $~ x :: IsA BitRecord)

-- ** Record composition

-- | Combine two 'BitRecord's to form a new 'BitRecord'. If the parameters are
-- not of type 'BitRecord' they will be converted.
data (:+^) (l :: BitRecord) (r :: IsA BitRecord) :: IsA BitRecord
infixl 3 :+^
type instance Eval (l :+^ r) = l `Append` Eval r

-- | Combine two 'BitRecord's to form a new 'BitRecord'. If the parameters are
-- not of type 'BitRecord' they will be converted.
data (:^+) (l :: IsA BitRecord) (r :: BitRecord) :: IsA BitRecord
infixl 3 :^+
type instance Eval (l :^+ r) = Eval l `Append` r

-- | Combine two 'BitRecord's to form a new 'BitRecord'. If the parameters are
-- not of type 'BitRecord' they will be converted.
type (:+:) (l :: BitRecord) (r :: BitRecord) = ((l `Append` r) :: BitRecord)
infixl 3 :+:

type family Append (l :: BitRecord) (r :: BitRecord) :: BitRecord where
  Append l 'EmptyBitRecord = l
  Append 'EmptyBitRecord r = r
  Append l r = 'BitRecordAppend l r

-- | Append a 'BitRecord' and a 'BitRecordField'
type (:+.) (r :: BitRecord)
           (f :: IsA (BitRecordField t1))
           = Append r ('BitRecordMember f)
infixl 6 :+.

-- | Append a 'BitRecordField' and a 'BitRecord'
type (.+:) (f :: IsA (BitRecordField t1))
           (r :: BitRecord)
  = Append ('BitRecordMember f) r
infixr 6 .+:

-- | Append a 'BitRecordField' and a 'BitRecordField' forming a 'BitRecord' with
-- two members.
type (.+.) (l :: IsA (BitRecordField t1))
           (r :: IsA (BitRecordField t2))
           = Append ('BitRecordMember l) ('BitRecordMember r)
infixr 6 .+.

-- | Set a field to either a static, compile time, value or a dynamic, runtime value.
type family (:~)
  (field :: IsA (BitRecordField (t :: BitField (rt :: Type) (st :: k) (len :: Nat))))
  (value :: IsA (FieldValue (label :: Symbol) st))
  :: IsA (BitRecordField t) where
  fld :~ StaticFieldValue l v  = (l @: fld) := v
  fld :~ RuntimeFieldValue l = l @: fld
infixl 7 :~

-- | Like ':~' but for a 'Maybe' parameter. In case of 'Just' it behaves like ':~'
-- in case of 'Nothing' it return an 'EmptyBitRecord'.
type family (:~?)
  (fld :: IsA (BitRecordField (t :: BitField (rt :: Type) (st :: k) (len :: Nat))))
  (value :: Maybe (IsA (FieldValue (label :: Symbol) st)))
  :: IsA BitRecord where
  fld :~? ('Just v) = RecordField (fld :~ v)
  fld :~? 'Nothing  = Pure 'EmptyBitRecord
infixl 7 :~?

-- | Like ':~' but for a 'Maybe' parameter. In case of 'Just' it behaves like ':~'
-- in case of 'Nothing' it return an 'EmptyBitRecord'.
type family (:+?)
  (fld :: IsA (BitRecordField (t :: BitField (rt :: Type) (st :: k) (len :: Nat))))
  (value :: Maybe (IsA (FieldValue (label :: Symbol) st)))
  :: BitRecord where
  fld :+? ('Just v) = 'BitRecordMember (fld :~ v)
  fld :+? 'Nothing  = 'EmptyBitRecord
infixl 7 :+?

-- | The field value parameter for ':~', either a static, compile time, value or
-- a dynamic, runtime value.
data FieldValue :: Symbol -> staticRep -> Type
data StaticFieldValue (label :: Symbol) :: staticRep -> IsA (FieldValue label staticRep)
data RuntimeFieldValue (label :: Symbol) :: IsA (FieldValue label staticRep)

-- *** Record Arrays and Repitition

-- | An array of records with a fixed number of elements, NOTE: this type is
-- actually not really necessary since 'ReplicateRecord' exists, but this allows
-- to have a different 'showRecord' output.
data RecArray :: BitRecord -> Nat -> IsA BitRecord

type r ^^ n = RecArray r n
infixl 5 ^^

type instance Eval (RecArray (r :: BitRecord) n ) = RecArrayToBitRecord r n

-- | Repeat a bit record @n@ times.
type family RecArrayToBitRecord (r :: BitRecord) (n :: Nat) :: BitRecord where
  RecArrayToBitRecord r 0 = 'EmptyBitRecord
  RecArrayToBitRecord r 1 = r
  RecArrayToBitRecord r n = Append r (RecArrayToBitRecord r (n - 1))

-- *** Lists of Records

-- | Let type level lists also be records
data
    BitRecordOfList
      (f  :: IsA (foo :-> BitRecord))
      (xs :: [foo])
      :: IsA BitRecord

type instance Eval (BitRecordOfList f xs) =
  FoldMap BitRecordAppendFun 'EmptyBitRecord f xs

type BitRecordAppendFun = Fun1 BitRecordAppendFun_
data BitRecordAppendFun_ :: BitRecord -> IsA (BitRecord :-> BitRecord)
type instance (BitRecordAppendFun_ l) $~ r = Append l r

-- *** Maybe Record

-- | Either use the value from @Just@ or return a 'EmptyBitRecord' value(types(kinds))
data OptionalRecord :: Maybe BitRecord -> IsA BitRecord
type instance Eval (OptionalRecord ('Just t)) = t
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
    MkFieldBits    :: forall (n :: Nat) . BitField (B n) Nat n
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

type Flag     = MkField 'MkFieldFlag
type Field n  = MkField ('MkFieldBits :: BitField (B n) Nat n)
type FieldU8  = MkField 'MkFieldU8
type FieldU16 = MkField 'MkFieldU16
type FieldU32 = MkField 'MkFieldU32
type FieldU64 = MkField 'MkFieldU64
type FieldI8  = MkField 'MkFieldI8
type FieldI16 = MkField 'MkFieldI16
type FieldI32 = MkField 'MkFieldI32
type FieldI64 = MkField 'MkFieldI64

-- | A data type for 'Field' and 'MkFieldBits', that is internally a 'Word64'.
-- It carries the number of relevant bits in its type.
newtype B (size :: Nat) = B {unB :: Word64}
  deriving (Read,Show,Num,Integral,Bits,FiniteBits,Eq,Ord,Bounded,Enum,Real)

instance (PrintfArg Word64, n <= 64) => PrintfArg (B n) where
  formatArg (B x) = formatArg x
  parseFormat (B x) = parseFormat x

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
type instance Eval (MaybeField ('Just  fld)) = 'BitRecordMember fld
type instance Eval (MaybeField 'Nothing) = 'EmptyBitRecord

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
   PrettyRecord ('BitRecordAppend l r) = PrettyRecord l <$$> PrettyRecord r

type instance ToPretty (f :: IsA (BitRecordField t)) = PrettyField f

type family PrettyField (f :: IsA (BitRecordField (t :: BitField (rt :: Type) (st :: Type) (size :: Nat)))) :: PrettyType where
  PrettyField (MkField t) = PrettyFieldType t
  PrettyField ((f :: IsA (BitRecordField t)) := v) =
    PrettyField f <+> PutStr ":=" <+> PrettyFieldValue t v
  PrettyField (LabelF l f) = l <:> PrettyField f

type family PrettyFieldType (t :: BitField (rt :: Type) (st :: Type) (size :: Nat)) :: PrettyType where
  PrettyFieldType ('MkFieldFlag) = PutStr "boolean"
  PrettyFieldType ('MkFieldBits :: BitField (B (s :: Nat)) Nat s) = PutStr "bits" <++> PrettyParens (PutNat s)
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
  PrettyFieldValue ('MkFieldBits :: BitField (B (s :: Nat)) Nat s) v =
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
