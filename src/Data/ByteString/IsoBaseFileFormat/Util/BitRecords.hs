{-# LANGUAGE UndecidableInstances #-}
module Data.ByteString.IsoBaseFileFormat.Util.BitRecords where

import Data.Kind
import Data.Word
import Data.Type.Bool
import GHC.TypeLits
import Data.Bits
import Data.Proxy
import Test.TypeSpecCrazy

data Field :: Nat -> Type
data (:=>) :: label -> Type -> Type
data (:*:) :: Type -> Type -> Type
type FieldPosition = (Nat, Nat)

-- | Scheme like @(cond ...)@ block
data Cond
  :: label
  -> [Type -> Maybe Type]
  -- ^ condition wich get the value for the label and -- the first that returns --
  -- @'Just r@' will terminate -- the 'Cond'
  -> (Type -> Type)
  -- ^ Fall back function
  -> Type 


type Flag = Field 1

infixr 6 :=>
infixl 5 :*:

-- nested fields
data (:/) :: Symbol -> k -> Type
infixr 7 :/


type family
  GetFieldSize (f :: l) :: Nat where
  GetFieldSize (label :=> f) = GetFieldSize f
  GetFieldSize (Field n ) = n
  GetFieldSize (l :*: r) = GetFieldSize l + GetFieldSize r

type family
  HasField (f :: fk) (l :: lk) :: Bool where
  HasField (l :=> f) l = 'True
  HasField (l :=> f) (l :/ p) = HasField f p
  HasField (f1 :*: f2) l = HasField f1 l || HasField f2 l
  HasField f l = 'False

type family
  HasFieldConstraint (label :: lk) (field :: fk) :: Constraint where
  HasFieldConstraint l f =
      If (HasField f l)
         (HasField f l ~ 'True)
         (TypeError ('Text "Label not found: '"
                     ':<>: 'ShowType l
                     ':<>: 'Text "' in:"
                     ':$$: 'ShowType f ))

type family
  FocusOn (l :: lk) (f :: fk) :: Result fk where
    FocusOn l f =
      If (HasField f l)
         ('Right (FocusOnUnsafe l f))
         ('Left ('Text "Label not found. Cannot focus '"
                     ':<>: 'ShowType l
                     ':<>: 'Text "' in:"
                     ':$$: 'ShowType f ))

type family
  FocusOnUnsafe (l :: lk) (f :: fk) :: fk where
  FocusOnUnsafe l        (l :=> f) = f
  FocusOnUnsafe (l :/ p) (l :=> f) = FocusOnUnsafe p f
  FocusOnUnsafe l        (f :*: f') = FocusOnUnsafe l (If (HasField f l) f f')

-- field location and access

type family
  GetFieldPosition (f :: field) (l :: label) :: Result FieldPosition where
  GetFieldPosition f l =
     If (HasField f l)
       ('Right (GetFieldPositionUnsafe f l))
       ('Left ('Text "Label not found. Cannot get bit range for '"
          ':<>: 'ShowType l
          ':<>: 'Text "' in:"
          ':$$: 'ShowType f ))

type family
  GetFieldPositionUnsafe (f :: field) (l :: label) :: FieldPosition where
  GetFieldPositionUnsafe (l :=> f)  l        = '(0, GetFieldSize f - 1)
  GetFieldPositionUnsafe (l :=> f)  (l :/ p) = GetFieldPositionUnsafe f p
  GetFieldPositionUnsafe (f :*: f') l        =
     If (HasField f l)
      (GetFieldPositionUnsafe f l)
      (AddToFieldPosition (GetFieldSize f) (GetFieldPositionUnsafe f' l))

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

type family
  AlignField (a :: Nat) (f :: field) :: Result field where
  AlignField 0 f = 'Left ('Text "Invalid alignment of 0")
  AlignField a f = 'Right (AddPadding ((a - (GetFieldSize f `Rem` a)) `Rem` a) f)

type family
  AddPadding (n :: Nat) (f :: field) :: field where
  AddPadding 0 f = f
  AddPadding n f = f :*: Field n

-- | Get the remainder of the integer division of x and y, such that @forall x
-- y. exists k. (Rem x y) == x - y * k@ The algorithm is: count down x
-- until zero, incrementing the accumulator at each step. Whenever the
-- accumulator is equal to y set it to zero.
--
-- If the accumulator has reached y reset it. It is important to do this
-- BEFORE checking if x == y and then returning the accumulator, for the case
-- where x = k * y with k > 0. For example:
--
-- @
--  6 `Rem` 3     = RemImpl 6 3 0
--  RemImpl 6 3 0 = RemImpl (6-1) 3 (0+1)   -- RemImpl Clause 4
--  RemImpl 5 3 1 = RemImpl (5-1) 3 (1+1)   -- RemImpl Clause 4
--  RemImpl 4 3 2 = RemImpl (4-1) 3 (2+1)   -- RemImpl Clause 4
--  RemImpl 3 3 3 = RemImpl 3 3 0           -- RemImpl Clause 2 !!!
--  RemImpl 3 3 0 = 0                       -- RemImpl Clause 3 !!!
-- @
--
type Rem (x :: Nat) (y :: Nat) = RemImpl x y 0
type family
  RemImpl (x :: Nat) (y :: nat) (acc :: Nat) :: Nat where
  -- finished if x was < y:
  RemImpl 0 y acc = acc
  RemImpl x y y   = RemImpl x y 0
  -- finished if x was >= y:
  RemImpl y y acc = acc
  -- the base case
  RemImpl x y acc = RemImpl (x - 1) y (acc + 1)

getFlag
  :: forall a (path :: k) (first :: Nat) field p1 p2
  . ( IsFieldC path field first first
    , Bits a )
   => p1 path -> p2 field -> a -> Bool
getFlag _ _ a = testBit a pos
    where pos = fromIntegral $ natVal (Proxy :: Proxy first)

setFlag
  :: forall a (path :: k) (first :: Nat) field p1 p2
  . ( IsFieldC path field first first
    , Bits a )
   => p1 path -> p2 field -> Bool -> a -> a
setFlag _ _ v a = modifyBit a pos
    where pos = fromIntegral $ natVal (Proxy :: Proxy first)
          modifyBit = if v then setBit else clearBit

getField
  :: forall a b (path :: k) (first :: Nat) (last :: Nat) field pxy1 pxy2
  . ( IsFieldC path field first last
    , Integral a
    , Bits a
    , Num b)
   => pxy1 path -> pxy2 field -> a -> b
getField _ _ a = fromIntegral ((a `shiftR` posFirst) .&. bitMask)
    where
      bitMask =
        let bitCount = 1 + posLast - posFirst
            in (2 ^ bitCount) - 1
      posFirst = fromIntegral $ natVal (Proxy :: Proxy first)
      posLast = fromIntegral $ natVal (Proxy :: Proxy last)

setField
  :: forall a b (path :: k) (first :: Nat) (last :: Nat) field pxy1 pxy2
  . ( IsFieldC path field first last
    , Num a
    , Bits a
    , Integral b)
   => pxy1 path -> pxy2 field -> b -> a -> a
setField _ _ v x = (x .&. bitMaskField) .|. (v' `shiftL` posFirst)
    where
      v' = bitMaskValue .&. fromIntegral v
      bitMaskField = complement (bitMaskValue `shiftL` posFirst)
      bitMaskValue =
        let bitCount = 1 + posLast - posFirst
            in (2 ^ bitCount) - 1
      posFirst = fromIntegral $ natVal (Proxy :: Proxy first)
      posLast = fromIntegral $ natVal (Proxy :: Proxy last)



type Foo =
       "foo" :=> Flag
   :*:           Field 4
   :*: "bar" :=> Field 2
   :*:           Field 4
   :*: "baz" :=> Field 17

type IsFieldC name field first last =
    ( name `HasFieldConstraint` field
     , KnownNat first
     , KnownNat last
     , 'Right '(first, last) ~ (GetFieldPosition field name)
     )

getFooField :: IsFieldC name Foo first last
   => proxy name -> Word64 -> Word64
getFooField px = getField px (Proxy :: Proxy Foo)
