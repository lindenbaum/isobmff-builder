{-# LANGUAGE UndecidableInstances #-}
module Data.ByteString.IsoBaseFileFormat.Util.BitsBuilder where

import Data.Kind
import Data.Word
import Data.Type.Bool
import GHC.TypeLits
import Data.Bits
import Data.Proxy
import Data.Typeable


-----------------------------------------

type WithError a = Either ErrorMessage a

type family Try (e :: WithError r) :: r where
  Try ('Right r) = r
  Try ('Left m) = TypeError m

-----------------------------------------

data Flag :: Type
data Field :: Nat -> Type
data (:=>) :: k -> Type -> Type
data (:*:) :: Type -> Type -> Type
type FieldPosition = (Nat, Nat)

infixr 6 :=>
infixl 5 :*:

-- nested fields
data (:/) :: Symbol -> k -> Type
infixr 7 :/


type family
  GetFieldSize (f :: l) :: Nat where
  GetFieldSize (label :=> f) = GetFieldSize f
  GetFieldSize Flag = 1
  GetFieldSize (Field n ) = n
  GetFieldSize (l :*: r) = GetFieldSize l + GetFieldSize r

type family
  HasField (f :: fk) (l :: lk) :: Bool where
  HasField (l :=> f) l = 'True
  HasField (l :=> f) (l :/ p) = HasField f p
  HasField (f1 :*: f2) l = HasField f1 l || HasField f2 l
  HasField f l = 'False

type family
  IsFieldOf (label :: lk) (field :: fk) :: Constraint where
  IsFieldOf l f =
      If (HasField f l)
         (HasField f l ~ 'True)
         (TypeError ('Text "Label not found: '"
                     ':<>: 'ShowType l
                     ':<>: 'Text "' in:"
                     ':$$: 'ShowType f ))

type family
  FocusOn (l :: lk) (f :: fk) :: WithError fk where
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
  GetFieldPosition (f :: field) (l :: label) :: WithError FieldPosition where
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
  AlignField (a :: Nat) (f :: field) :: WithError field where
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
    ( name `IsFieldOf` field
     , KnownNat first
     , KnownNat last
     , 'Right '(first, last) ~ (GetFieldPosition field name)
     )


getFooField :: IsFieldC name Foo first last
   => proxy name -> Word64 -> Word64
getFooField px = getField px (Proxy :: Proxy Foo)

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

type SmallPacket = Flag :*: Field 7
testFieldSize :: Expect (GetFieldSize SmallPacket `SameType` 8)
testFieldSize = Expect

--

type TestHasField =
       "foo" :=> Flag
   :*:           Field 4
   :*: "bar" :=> Field 2
testBitHasFields
  :: ExpectTrue (HasField TestHasField "foo" && HasField TestHasField "bar")
testBitHasFields = ExpectTrue

type TestHasNestedField =
       "foo" :=> "bar" :=> Field 2
testBitHasNestedFields
  :: ExpectTrue (HasField TestHasNestedField ("foo" :/ "bar"))
testBitHasNestedFields = ExpectTrue

--

type TestFocus =
       "bad" :=> Field 13
   :*: "bar" :=> (              Field 7
                  :*: "bax" :=> Flag )
   :*: "foo" :=> ExpectedFocus

type ExpectedFocus =
  (     "fmm" :=> Flag
                  :*:           Flag
                  :*:           Flag)

testFocus
  :: Expect (FocusOn "foo" TestFocus `SameType` 'Right ExpectedFocus)
testFocus = Expect

type ExpectedFocusError =
  'Left
   (       'Text "Label not found. Cannot focus '"
     ':<>: 'ShowType "xxx"
     ':<>: 'Text "' in:"
     ':$$: 'ShowType
            (      "bad" :=> Field 13
               :*: "bar" :=> (Field 7 :*: "bax" :=> Flag)
               :*: "foo" :=> ExpectedFocus))

testFocusError
  :: Expect (FocusOn "xxx" TestFocus `SameType` ExpectedFocusError)
testFocusError = Expect

testFocusConstraintCheck
  :: Try (FocusOn "xxx" TestFocus) ~ f => Proxy f
testFocusConstraintCheck = Proxy

--

testRem
  :: Expect ( Rem 0 3 `SameType` 0
            , Rem 1 3 `SameType` 1
            , Rem 2 3 `SameType` 2
            , Rem 3 3 `SameType` 0
            , Rem 4 3 `SameType` 1
            , Rem 5 3 `SameType` 2
            , Rem 6 3 `SameType` 0
            )
testRem = Expect

--

testAlign
  :: Expect ( Try (AlignField 7 Flag)       `SameType`  (Flag :*: Field 6)
            , Try (AlignField 1 Flag)       `SameType`  Flag
            , Try (AlignField 8 (Field 7))  `SameType`  (Field 7 :*: Field 1)
            , Try (AlignField 8 (Field 8))  `SameType`  Field 8
            , Try (AlignField 8 (Field 9))  `SameType`  (Field 9 :*: Field 7)
            )
testAlign = Expect

--

type TestField0 =  "test" :=> Field 19
testFieldPosition0
   :: Expect ((GetFieldPositionUnsafe TestField0 "test") `SameType` '(0,18))
testFieldPosition0 = Expect

type TestField1 =
      Field 1
  :*: "foo" :=> Flag
  :*: Field 8
  :*: "bar" :=> Field 5
  :*: "baz" :=> Field 9

testFieldPosition1Foo
   :: Expect ((GetFieldPositionUnsafe TestField1 "foo") `SameType` '(1,1))
testFieldPosition1Foo = Expect

testFieldPosition1Bar
   :: Expect ((GetFieldPositionUnsafe TestField1 "bar") `SameType` '(10,14))
testFieldPosition1Bar = Expect

testFieldPosition1Baz
   :: Expect (Try (GetFieldPosition TestField1 "baz") `SameType` '(15,23))
testFieldPosition1Baz = Expect

-- (Proxy :: Proxy ("bar" :/ "foo"))
type TestFieldNested =
     Field 13
     :*: "bar" :=> (              Field 7
                    :*:           Flag
                    :*: "foo" :=> Field 16
                    :*:           Flag
                    :*:           Flag)

testFieldPositionTestFieldNested
   :: Expect
       (Try
         (GetFieldPosition TestFieldNested ("bar" :/ "foo"))
             `SameType`
                        '(13 + 7 + 1, (16 - 1) + (13 + 7 + 1)))
testFieldPositionTestFieldNested = Expect

--

testFieldPositionToList
   :: Expect
       (FieldPostitionToList
         '(15,23)
       `SameType`
       '[15,16,17,18,19,20,21,22,23])
testFieldPositionToList = Expect

-- Wont compile
-- testFieldPositionToListErr :: Expect (IsFieldPostition '(1,0))
-- testFieldPositionToListErr = Expect

--

testFlagFoo0 =
 not
   (getFlag
      (Proxy :: Proxy "foo")
      (Proxy :: Proxy ("no" :=> Flag :*: "foo" :=> Flag ))
      (0xd :: Word8))


testFlagFoo1 =
  getFlag
      (Proxy :: Proxy "foo")
      (Proxy :: Proxy ("no" :=> Flag :*: "foo" :=> Flag ))
      (0xf :: Word8)

--

testGetField0 =
  getField
      (Proxy :: Proxy "foo")
      (Proxy :: Proxy (Field 16 :*: "foo" :=> Flag))
      (0x00010000 :: Word32)
      == (1 :: Word32)

testGetField1 =
  getField
      (Proxy :: Proxy "foo")
      (Proxy :: Proxy (Field 13 :*: "foo" :=> Field 16))
      (0xcafe0000 `shiftR` 3 :: Word32)
      == (0xcafe :: Word32)

testGetField2 =
  getField
      (Proxy :: Proxy "foo")
      (Proxy :: Proxy (Field 13
                       :*: "foo" :=> (    Field 7
                                      :*: Flag
                                      :*: Field 6
                                      :*: Flag
                                      :*: Flag)))
      (0xcafe0000 `shiftR` 3 :: Word32)
      == (0xcafe :: Word32)

testGetField3 =
  getField
      (Proxy :: Proxy ("bar" :/ "foo"))
      (Proxy :: Proxy (              Field 13
                       :*: "bar" :=> (              Field 7
                                      :*:           Flag
                                      :*: "foo" :=> Field 16
                                      :*:           Flag
                                      :*:           Flag)))
      (0xcafe `shiftL` (13 + 7 + 1) :: Word64)
      == (0xcafe :: Word64)

testSetFlag =
  setFlag
      (Proxy :: Proxy ("bar" :/ "foo"))
      (Proxy :: Proxy (              Field 13
                       :*: "bar" :=> (              Field 7
                                      :*:           Flag
                                      :*: "foo" :=> Flag
                                      :*:           Flag
                                      :*:           Flag)))
      True
      (0 :: Word64)
      == (1 `shiftL` (13 + 7 + 1) :: Word64)

testSetField1 =
  setField
      (Proxy :: Proxy ("bar" :/ "foo"))
      (Proxy :: Proxy (              Field 13
                       :*: "bar" :=> (              Field 7
                                      :*:           Flag
                                      :*: "foo" :=> Field 4
                                      :*:           Flag
                                      :*:           Flag)))
      (0xe :: Word8)
      (0xcaf0 `shiftL` (13 + 7 + 1) :: Word64)
      == (0xcafe `shiftL` (13 + 7 + 1) :: Word64)



-- -----------------------------------------------------------------------------

type family SameType a b :: Constraint where
  SameType a a = ()
  SameType a b = TypeError (      'Text "Expectation failed!"
                            ':$$: 'Text "    Actual: "
                            ':<>: 'ShowType a
                            ':$$: 'Text " Exptected: "
                            ':<>: 'ShowType b)

type family ShouldBeTrue a (b::Bool) :: Constraint where
  ShouldBeTrue a 'True = ()
  ShouldBeTrue a b = TypeError
                           (      'Text "Expectation failed!"
                            ':$$: 'Text "Expected "
                            ':<>: 'ShowType a
                            ':$$: 'Text "to be true." )

data Expect (c :: Constraint) where
  Expect :: c => Expect c
  Expect' :: c => String -> Expect c

instance Typeable c => Show (Expect c) where
  show px@Expect = "SUCCESS: Constriant satisfied - " ++ show (typeRep px)
  show px@(Expect' m) = "SUCCESS: " ++ m ++ " - " ++ show (typeRep px)

data ExpectTrue (c :: k) where
  ExpectTrue :: ShouldBeTrue c 'True => ExpectTrue c
  ExpectTrue' :: ShouldBeTrue c 'True => String -> ExpectTrue c

instance Typeable c => Show (ExpectTrue c) where
  show px@ExpectTrue = "SUCCESS: Type is 'True - " ++ show (typeRep px)
  show px@(ExpectTrue' m) = "SUCCESS: " ++ m ++ " - " ++ show (typeRep px)
