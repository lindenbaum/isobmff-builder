{-# LANGUAGE UndecidableInstances #-}
module Data.Type.BitRecords.Core where

import Data.Bits
import Data.Int
import Data.Kind (Type, Constraint)
import Data.Proxy
import Data.Type.BitRecords.Arithmetic
import Data.Type.Bool
import Data.Type.Pretty
import Data.Word
import GHC.TypeLits
import Test.TypeSpecCrazy

-- * Records

-- ** Record ADT

-- | 'BitRecordField's assembly
data BitRecord where
  BitRecordMember           :: BitRecordField -> BitRecord
  AppendedBitRecords        :: BitRecord      -> BitRecord -> BitRecord
  ExtraPrettyPrintingOutput :: PrettyType     -> BitRecord
  EmptyBitRecord            :: BitRecord
  -- TODO  MissingBitRecord          :: ErrorMessage     -> BitRecord

-- | Type class of types that can be represented as a 'BitRecord'
type family ToBitRecord (t :: k) :: BitRecord
type instance ToBitRecord (x :: BitRecord) = x

-- | A 'BitRecord' that represents a placeholder for a future 'BitRecord'
type NoBitRecord = ('Nothing :: Maybe BitRecord)

-- *** Basic Accessor

-- | Extract the size in as a number of bits from a 'BitRecord'
type family BitRecordSize (x :: BitRecord) :: Nat where
  BitRecordSize ('BitRecordMember f)           = BitRecordFieldSize f
  BitRecordSize ('AppendedBitRecords l r)      = BitRecordSize l + BitRecordSize r
  BitRecordSize ('ExtraPrettyPrintingOutput a) = 0
  BitRecordSize 'EmptyBitRecord                = 0

-- | The total number of members in a record.
type family BitRecordMemberCount (b :: BitRecord) :: Nat where
  BitRecordMemberCount ('BitRecordMember f)           = 1
  BitRecordMemberCount ('AppendedBitRecords l r)      = BitRecordMemberCount l + BitRecordMemberCount r
  BitRecordMemberCount ('ExtraPrettyPrintingOutput a) = 0
  BitRecordMemberCount 'EmptyBitRecord                = 0

-- | Return the size of the record.
getRecordSizeFromProxy
  :: forall px (rec :: BitRecord) . KnownNat (BitRecordSize rec) => px rec -> Integer
getRecordSizeFromProxy _ = natVal (Proxy :: Proxy (BitRecordSize rec))

-- ** Field ADT

-- | A family of bit fields.
--
-- A bit field always has a size, i.e. the number of bits it uses, as well as a
-- term level value type and a type level value type. It also has an optional
-- label, and an optional value assigned to it.
data BitRecordField where
  BitRecordField ::
    Maybe Symbol -> Type -> Nat -> Maybe t -> BitRecordField

-- | A 'BitRecordField' can be used as 'BitRecordMember'
type instance ToBitRecord (a :: BitRecordField) = 'BitRecordMember a

-- | Type class of types that can be represented as a 'BitRecordField'
type family ToBitRecordField (t :: k) :: BitRecordField
type instance ToBitRecordField (x :: BitRecordField) = x

-- | Extract the size in as a number of bits from a 'BitRecordField'
type family BitRecordFieldSize (x :: BitRecordField) where
  BitRecordFieldSize ('BitRecordField label v size dt) = size

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

type instance ToBitRecord (RecArray r n) = RecArrayToBitRecord r n

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

-- *** Length + Array/List

-- | A record with a /size/ member, and a nested record that can be counted using 'SizeFieldValue'.
data Sized :: SizeField -> k -> Type

type instance ToBitRecord (Sized sf r) =
  'AppendedBitRecords (ToBitRecord (ToBitRecordField sf)) (ToBitRecord r)
type instance SizeFieldValue (b :: BitRecord) = BitRecordMemberCount b

-- ** Field Constructor

-- *** Setting a Label

-- | A field with a label assigned to it.
--
-- The left argument is converted to a 'BitRecordField' via 'ToBitRecordField'
-- in necessary. The right argument must match the parameter of the 'Maybe' of
-- the corresponding field in 'BitRecordField'.
type family (:=>) (l :: Symbol) (b :: BitRecordField) :: BitRecordField where
  (:=>) label ('BitRecordField oldLabel demoteRep size value) =
    'BitRecordField ('Just label) demoteRep size value
infixr 5 :=>

-- *** Assignment

-- | A field with a (type-level-) value assigned to.
--
-- The left argument is converted to a 'BitRecordField' via 'ToBitRecordField'
-- in necessary. The right argument must match the parameter of the 'Maybe' of
-- the corresponding field in 'BitRecordField'.
type family (:=) (b :: BitRecordField) (v :: k) :: BitRecordField where
  (:=) ('BitRecordField label demoteRep size 'Nothing) v =
    'BitRecordField label demoteRep size ('Just v)
infixr 6 :=

-- *** Primitive Types

-- | A single bit (boolean) field
type Flag = 'BitRecordField 'Nothing Bool 1 ('Nothing :: Maybe Bool)

type instance ToBitRecordField (x :: Bool) = 'BitRecordField 'Nothing Bool 1 ('Just x)

type instance ToBitRecordField Bool = Flag

-- | A single bit (Boolean) record
type instance ToBitRecord Bool = ToBitRecord Flag

-- | A single bit (Boolean) record
type instance ToBitRecord (x :: Bool) = 'BitRecordMember (ToBitRecordField x)

-- | Define a field of bits with a size
type Field n = 'BitRecordField 'Nothing Word64 n ('Nothing :: Maybe Nat)

type FieldU8 =
  'BitRecordField 'Nothing Word8 8 ('Nothing :: Maybe Nat)

type FieldU16 =
  'BitRecordField 'Nothing Word16 16 ('Nothing :: Maybe Nat)

type FieldU32 =
  'BitRecordField 'Nothing Word32 32 ('Nothing :: Maybe Nat)

type FieldU64 =
  'BitRecordField 'Nothing Word64 64 ('Nothing :: Maybe Nat)

-- | A signed field value.
data SignedNat where
  PositiveNat :: Nat -> SignedNat
  NegativeNat :: Nat -> SignedNat

type FieldI8 =
  'BitRecordField 'Nothing Int8 8 ('Nothing :: Maybe SignedNat)

type FieldI16 =
  'BitRecordField 'Nothing Int16 16 ('Nothing :: Maybe SignedNat)

type FieldI32 =
  'BitRecordField 'Nothing Int32 32 ('Nothing :: Maybe SignedNat)

type FieldI64 =
  'BitRecordField 'Nothing Int64 64 ('Nothing :: Maybe SignedNat)

type instance ToBitRecordField 'NoSizeField =
  'BitRecordField 'Nothing () 0 ('Nothing :: Maybe ())

type instance ToBitRecordField 'SizeField8 =
  'BitRecordField 'Nothing Word8 8 ('Nothing :: Maybe Nat)

type instance ToBitRecordField 'SizeField16 =
  'BitRecordField 'Nothing Word16 16 ('Nothing :: Maybe Nat)

type instance ToBitRecordField 'SizeField32 =
  'BitRecordField 'Nothing Word32 32 ('Nothing :: Maybe Nat)

-- *** Composed Fields

-- | A Flag (1-bit) that is true if the type level maybe is 'Just'.
type family FlagJust a :: BitRecordField where
  FlagJust ('Just x) = ToBitRecordField 'True
  FlagJust 'Nothing  = ToBitRecordField 'False

-- | A Flag (1-bit) that is true if the type level maybe is 'Nothing'.
type family FlagNothing a :: BitRecordField where
  FlagNothing ('Just x) = ToBitRecordField 'False
  FlagNothing 'Nothing  = ToBitRecordField 'True

-- *** Size Fields

-- | Indicate the size of a length field, e.g. for 'SizedString' or 'Sized'
data SizeField = NoSizeField | SizeField8 | SizeField16 | SizeField32

-- | For something to be augmented by a size field there must be an instance of
-- this family to generate the value of the size field, e.g. by counting the
-- elements.
type family SizeFieldValue (c :: k) :: Nat

-- | A field that renders to the length of a 'SizedString' using the given
-- word type for the size.
data ToStringLength :: Type -> Type -> Type -- TODO move

-- ** member location and access

-- | Return 'True' a record has a field with a given label
type family
  HasField (r :: BitRecord) (l :: Symbol) :: Bool where
  HasField ('BitRecordMember ('BitRecordField ('Just l) d s md)) l = 'True
  HasField ('AppendedBitRecords a b) l                             = HasField a l || HasField b l
  HasField f l                                                     = 'False

type FieldPosition = (Nat, Nat)

type family
  GetFieldPosition (r :: BitRecord) (l :: Symbol) :: Result FieldPosition where
  GetFieldPosition f l =
     If (HasField f l)
       ('Right (GetFieldPositionUnsafe f l))
       ('Left ('Text "Label not found. Cannot get bit range for '"
          ':<>: 'ShowType l
          ':<>: 'Text "' in:"
          ':$$: 'ShowType f ))

type family
  GetFieldPositionUnsafe (r :: BitRecord) (l :: Symbol) :: FieldPosition where
  GetFieldPositionUnsafe ('BitRecordMember ('BitRecordField ('Just l) d 0 md)) l =
    TypeError ('Text "Cannot get position of zero width field: " -- TODO by this type error this function is partial, fix it by making it  return Maybe FieldPosition
               ':<>: 'ShowType ('BitRecordMember ('BitRecordField ('Just l) d 0 md)))
  GetFieldPositionUnsafe ('BitRecordMember ('BitRecordField ('Just l) d s md)) l = '(0, s - 1)
  GetFieldPositionUnsafe ('AppendedBitRecords f f') l =
     If (HasField f l)
      (GetFieldPositionUnsafe f l)
      (AddToFieldPosition (BitRecordSize f) (GetFieldPositionUnsafe f' l))

type family
  AddToFieldPosition (v :: Nat) (e :: (Nat, Nat)) :: (Nat, Nat) where
  AddToFieldPosition v '(a,b) = '(a + v, b + v)

type family
  FieldPostitionToList (pos :: FieldPosition) :: [Nat] where
    FieldPostitionToList '(a, a) = '[a]
    FieldPostitionToList '(a, b) = (a ': (FieldPostitionToList '(a+1, b)))

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

type HasFieldLabelC field record first last =
    ( RunConstraintE (record `HasFieldConstraint` field)
     , KnownNat first
     , KnownNat last
     , 'Right '(first, last) ~ (GetFieldPosition record field)
     )

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

-- ** Auto-Padding

type Align padRight a f =
    AddPadding padRight ((a - (BitRecordSize f `Rem` a)) `Rem` a) f

type family
  AddPadding (padRight :: Bool) (n :: Nat) (r :: rk) :: rk where
  AddPadding padRight 0 r = r
  AddPadding 'True n r = r :>: Field n := 0
  AddPadding 'False n r = Field n := 0 :>: r

-- * Value-level accessors

getField
  :: forall a b field (first :: Nat) (last :: Nat) record pxy1 pxy2
  . ( HasFieldLabelC field record first last
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
  . ( HasFieldLabelC field record first last
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

-- | Return the value of a single bit field as Bool
getFlag
  :: forall a (field :: Symbol) (first :: Nat) record p1 p2
  . ( HasFieldLabelC field record first first
    , Bits a )
   => p1 field -> p2 record -> a -> Bool
getFlag _ _ a = testBit a pos
    where pos = fromIntegral $ natVal (Proxy :: Proxy first)

setFlag
  :: forall a field (first :: Nat) record p1 p2
  . ( HasFieldLabelC field record first first
    , Bits a )
   => p1 field -> p2 record -> Bool -> a -> a
setFlag _ _ v a = modifyBit a pos
    where pos = fromIntegral $ natVal (Proxy :: Proxy first)
          modifyBit = if v then setBit else clearBit

-- * Field and Record PrettyType Instances

-- | Render @rec@ to a pretty, human readable form. Internally this is a wrapper
-- around 'ptShow' using 'PrettyRecord'.
showRecord
  :: forall proxy (rec :: BitRecord) . PrettyTypeShow (PrettyRecord rec)
  => proxy rec -> String
showRecord _ = showPretty (Proxy :: Proxy (PrettyRecord rec))

type family PrettyRecord (rec :: BitRecord) :: PrettyType where
   PrettyRecord ('BitRecordMember m) = PrettyField m
   PrettyRecord 'EmptyBitRecord = 'PrettyEmpty
   PrettyRecord ('AppendedBitRecords l r) = PrettyRecord l <++> PrettyRecord r
   PrettyRecord ('ExtraPrettyPrintingOutput p) = p

type family PrettyField (f :: BitRecordField) :: PrettyType where
  PrettyField ('BitRecordField label demRep size value) =
     (ToPretty label <||> PutStr "_") <++> PutStr ":"
     <$$-->
       "size" <:> PutNat size     <$$>
       "type" <:> ToPretty demRep <$$>
       "size" <:> PutNat size     <$$>
       "value" <:> ToPretty value

-- * Constraint Utilities

-- | A wrapper around 'Constraint' that propagates 'TypeError'.
type ConstraintE = Either Constraint Constraint

-- | Unwrap a 'ConstraintE', this is where 'TypeError's might be /thrown/.
type family
  RunConstraintE t :: Constraint where
  RunConstraintE ('Left t) = t
  RunConstraintE ('Right t) = t
