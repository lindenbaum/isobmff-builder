{-# LANGUAGE UndecidableInstances #-}
module Data.Type.BitRecords.Core where

import Data.Type.BitRecords.Arithmetic
import Data.Kind
import Data.Type.Bool
import GHC.TypeLits
import Data.Bits
import Data.Proxy
import Test.TypeSpecCrazy
import Data.Type.Pretty

-- * Fields

-- | Define a field with a size
data Field :: Nat -> Type

-- | Alias for a single bit field
type Flag = Field 1

-- | Get the field size of a field -- TODO remove
type family GetFieldSize field :: Nat
type instance GetFieldSize (Field n) = n

type FieldPosition = (Nat, Nat)

-- * Records

-- | Combine two fields to form a new field.
--
-- This field composition happens in the order from TODO wrong DOC
-- the *most significant bit* to the *least significant bit*
--
-- @       MSB                                             LSB
--    Bit: |k  ..  k-(m+1)|k-m  ..  k-(m+n+1)| k-(m+n)  ..  0|
--  Value: \------f0-----/\--------f1--------/\--- empty ---/
-- @
--
data (:>:) :: Type -> Type -> Type
infixl 3 :>:

-- * Nested Records

-- | A field with a name
data (:=>) :: label -> Type -> Type where
infixr 5 :=>

-- | A field with a constant fixed value
data (:=) :: Type -> Nat -> Type
infixr 6 :=

-- | Make a part of the record ignored
data Ignore :: Type -> Type -- TODO remove

-- | A Path of field labels for nested record created with ':=>'
data (:/) :: Symbol -> label -> Type -- TODO implement nested record support
infixr 7 :/

-- | A wrapper around 'Constraint' that propagates 'TypeError'.
type ConstraintE = Either Constraint Constraint

-- | Unwrap a 'ConstraintE', this is where 'TypeError's might be /thrown/.
type family
  RunConstraintE t :: Constraint where
  RunConstraintE ('Left t) = t
  RunConstraintE ('Right t) = t

-- * BitRecord Accessor

-- | Return the size of the record.
getRecordSizeFromProxy
  :: forall px rec . KnownNat (GetRecordSize rec) => px rec -> Integer
getRecordSizeFromProxy _ = natVal (Proxy :: Proxy (GetRecordSize rec))

type family
  GetRecordSize (r :: rk) :: Nat where
  GetRecordSize (label :=> f) = GetRecordSize f
  GetRecordSize (l :>: r)     = GetRecordSize l + GetRecordSize r
  GetRecordSize (Ignore r)    = GetRecordSize r
  GetRecordSize (Field n)     = n
  GetRecordSize (r := v)      = GetRecordSize r

type family
  HasField (r :: rk) (l :: lk) :: Bool where
  HasField (l :=> f) l        = 'True
  HasField (l :=> f) (l :/ p) = HasField f p
  HasField (f1 :>: f2) l      = HasField f1 l || HasField f2 l
  HasField (r := v) l         = HasField r l
  HasField (Ignore r) l       = HasField r l
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

-- field location and access

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
  GetFieldPositionUnsafe (l :=> f)  l        = '(0, GetRecordSize f - 1)
  GetFieldPositionUnsafe (l :=> f)  (l :/ p) = GetFieldPositionUnsafe f p
  GetFieldPositionUnsafe (f := v)   l        = GetFieldPositionUnsafe f l
  GetFieldPositionUnsafe (Ignore f) l        = GetFieldPositionUnsafe f l
  GetFieldPositionUnsafe (f :>: f') l        =
     If (HasField f l)
      (GetFieldPositionUnsafe f l)
      (AddToFieldPosition (GetRecordSize f) (GetFieldPositionUnsafe f' l))

type family
  AddToFieldPosition (v :: Nat) (e :: (Nat, Nat)) :: (Nat, Nat) where
  AddToFieldPosition v '(a,b) = '(a + v, b + v)

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

type family
  FieldPostitionToList (pos :: FieldPosition) :: [Nat] where
    FieldPostitionToList '(a, a) = '[a]
    FieldPostitionToList '(a, b) = (a ': (FieldPostitionToList '(a+1, b)))

type Align padRight a f =
    AddPadding padRight ((a - (GetRecordSize f `Rem` a)) `Rem` a) f

type family
  AddPadding (padRight :: Bool) (n :: Nat) (r :: rk) :: rk where
  AddPadding padRight 0 r = r
  AddPadding 'True n r = r :>: Ignore (Field n := 0)
  AddPadding 'False n r = Ignore (Field n := 0) :>: r


-- * Bit record accessor for 'Num's

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

-- * PrettyType instances

-- | Render @rec@ to a pretty, human readable form. Internally this is a wrapper
-- around 'ptShow' using 'PrettyRecord'.
showRecord
  :: forall proxy (rec :: Type)
  . PrettyTypeShow (PrettyRecord rec)
  => proxy rec -> String
showRecord _ = ptShow (Proxy :: Proxy (PrettyRecord rec))

-- | A type family to pretty print @rec@ to a 'PrettyType'.
type family PrettyRecord rec :: PrettyType where
  PrettyRecord (Field 0) = 'PrettyEmpty
  PrettyRecord (Field 1) = PutStr "X"
  PrettyRecord (Field n) =
    PutStr "<" <++> PrettyOften (n - 2) (PutStr ".") <++> PutStr ">"
  PrettyRecord (l :=> r) =
    PutStr "<" <++>
    'PrettySymbol ('PrettyPadded ((GetRecordSize r) - 2)) ('PrettyPrecision ((GetRecordSize r) - 2)) l
    <++> PutStr ">"
  PrettyRecord (r :=  v) =
    'PrettyNat 'PrettyUnpadded ('PrettyPrecision (GetRecordSize r)) 'PrettyBit (v `Rem` (2 ^ (GetRecordSize r)))
  PrettyRecord (l :>: r) = PrettyRecord l <++> PrettyRecord r
