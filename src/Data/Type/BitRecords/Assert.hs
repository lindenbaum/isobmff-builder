{-# LANGUAGE UndecidableInstances #-}
-- | Type - level assertion utility leveraging custom 'TypeError's
module Data.Type.BitRecords.Assert
  ( type Assert, type Assertion, type Check
  , type (?::)
  , type (@&&)
  , type NatLE
  , type NatGE
  , type NatIn
  , type LengthIn
  , type CountElementsForLengthIn
  ) where

import GHC.TypeLits
import Data.Kind (type Type)
import Data.Type.Bool

-- | Assert that a given type satisfies some condition defined via 'Check'
-- instances. This is only an alias for 'Assert'
type (?::) x cond = Assert cond x
infixr 0 ?::

-- | Apply an 'Assertion' to some value and return that value if the assertion
-- holds, otherwise throw a 'TypeError'.
type family Assert (cond :: Assertion a) (x :: a) :: a where
  Assert cond x = ProcessCheckResult (Check cond x) x

-- | Make an assertion by creating a phantom data type which contains @TyFun a
-- (Maybe ErrorMessage)@ as last parameter. 'Check' is used by 'Assert' the
-- /apply/ the 'TyFun's which are specialized in this definition to have the
-- return type @Maybe ErrorMessage@.
type Assertion a = TyFun a (Maybe ErrorMessage) -> Type

-- | Apply 'Assertion's to the actual values.
type family Check (f :: Assertion a) (x :: a) :: Maybe ErrorMessage

type family ProcessCheckResult (r :: Maybe ErrorMessage) (x :: a) :: a where
  ProcessCheckResult 'Nothing x = x
  ProcessCheckResult ('Just blah) x =
    TypeError ('Text "Assertion on value " ':<>: 'ShowType x ':<>: 'Text " failed:" ':$$: blah)

-- | Shamelessly stolen from the singletons library.
data TyFun :: Type -> Type -> Type

-- | Assert that two assertions both hold
data (@&&) :: Assertion a -> Assertion a -> Assertion a

type instance Check (a1 @&& a2) x =
  (ProcessCheckResult (Check a2 x)
   (ProcessCheckResult (Check a1 x) x))

-- | Assert that a 'Nat' is greater than or equal to an other 'Nat'.
data NatGE :: Nat -> Assertion Nat

type instance Check (NatGE n) x =
  If (n <=? x) 'Nothing ('Just (     'Text "Natural too small: " ':<>: 'ShowType x
                               ':$$: 'Text "Required:          >=" ':<>: 'ShowType n))

-- | Assert that a 'Nat' is less than or equal to an other 'Nat'.
data NatLE :: Nat -> Assertion Nat

type instance Check (NatLE n) x =
  If (x <=? n) 'Nothing ('Just (     'Text "Natural too big: " ':<>: 'ShowType x
                               ':$$: 'Text "Required:         <=" ':<>: 'ShowType n))


-- | Assert that a 'Nat' is in a specific range.
data NatIn :: Nat -> Nat -> Assertion Nat

type instance Check (NatIn from to) n =
  CheckNatInRange (n <=? to) (from <=? n) from to n

type family
  CheckNatInRange
     (lt :: Bool) (gt :: Bool) (from :: Nat) (to :: Nat) (n :: Nat) :: Maybe ErrorMessage where
  CheckNatInRange 'True 'True from to x = 'Nothing
  CheckNatInRange c1 c2 from to x =
    'Just ('Text "Natural out of range: " ':<>: 'ShowType x
           ':<>: 'Text " not in "
           ':<>: 'ShowType from ':<>: 'Text " .. " ':<>: 'ShowType to)

-- | Assert that a list's length falls into a given range.
-- This is generalized from actual lists; everything with a 'CountElementsForLengthIn'
-- instance can be asserted.
data LengthIn :: Nat -> Nat -> Assertion a

type instance Check (LengthIn from to) xs = Check (NatIn from to) (CountElementsForLengthIn xs)

type family CountElementsForLengthIn (xs :: k) :: Nat
type instance CountElementsForLengthIn ('[]) = 0
type instance CountElementsForLengthIn (x ': xs) = 1 + CountElementsForLengthIn xs
