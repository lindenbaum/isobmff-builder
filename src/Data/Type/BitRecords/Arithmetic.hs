{-# LANGUAGE UndecidableInstances #-}
module Data.Type.BitRecords.Arithmetic where

import Data.Type.Bool
import GHC.TypeLits
import qualified Data.Promotion.Prelude.List as Sing

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
-- If @y@ is a power of 2 and @y <= 2^32@ then 'RemPow2' will be invoked.
type family Rem (x :: Nat) (y :: Nat) :: Nat where
  -- special optimizations for
  Rem x 4294967296 = RemPow2 x 32
  Rem x 2147483648 = RemPow2 x 31
  Rem x 1073741824 = RemPow2 x 30
  Rem x 536870912 = RemPow2 x 29
  Rem x 268435456 = RemPow2 x 28
  Rem x 134217728 = RemPow2 x 27
  Rem x 67108864 = RemPow2 x 26
  Rem x 33554432 = RemPow2 x 25
  Rem x 16777216 = RemPow2 x 24
  Rem x 8388608 = RemPow2 x 23
  Rem x 4194304 = RemPow2 x 22
  Rem x 2097152 = RemPow2 x 21
  Rem x 1048576 = RemPow2 x 20
  Rem x 524288 = RemPow2 x 19
  Rem x 262144 = RemPow2 x 18
  Rem x 131072 = RemPow2 x 17
  Rem x 65536 = RemPow2 x 16
  Rem x 32768 = RemPow2 x 15
  Rem x 16384 = RemPow2 x 14
  Rem x 8192 = RemPow2 x 13
  Rem x 4096 = RemPow2 x 12
  Rem x 2048 = RemPow2 x 11
  Rem x 1024 = RemPow2 x 10
  Rem x 512 = RemPow2 x 9
  Rem x 256 = RemPow2 x 8
  Rem x 128 = RemPow2 x 7
  Rem x 64 = RemPow2 x 6
  Rem x 32 = RemPow2 x 5
  Rem x 16 = RemPow2 x 4
  Rem x 8 = RemPow2 x 3
  Rem x 4 = RemPow2 x 2
  Rem x 2 = RemPow2 x 1
  Rem x 1 = 0
  Rem x 0 = TypeError ('Text "divide by zero: " ':<>: 'ShowType x ':<>: 'Text " `Rem` 0")
  Rem x y = RemImpl x y 0
type family
  RemImpl (x :: Nat) (y :: nat) (acc :: Nat) :: Nat where
  -- finished if x was < y:
  RemImpl 0 y acc = acc
  RemImpl x y y   = RemImpl x y 0
  -- finished if x was >= y:
  RemImpl y y acc = acc
  -- the base case
  RemImpl x y acc = RemImpl (x - 1) y (acc + 1)

-- | Efficient 'Rem' operation for power of 2 values. Note that x must be
-- representable by 'RemPow2Bits' bits.
type RemPow2 x p =
  FromBits (Sing.Reverse (Sing.Take p (Sing.Reverse (ToBits x RemPow2Bits))))

-- | Maximum number of bits an argument @x@ of 'RemPow2' may occupy.
type RemPow2Bits = 64

-- | Integer division of x and y: @Div x y  ==> x / y@,
-- NOTE This only works for small numbers currently
type Div (x :: Nat) (y :: Nat) = DivImpl (x - (x `Rem` y)) y 0
type family
  DivImpl (x :: Nat) (y :: nat) (acc :: Nat) :: Nat where
  DivImpl 0 y acc = acc
  DivImpl x y acc = If (x + 1 <=? y) acc (DivImpl (x - y) y (acc + 1))

-- * Bit manipulation

type family TestHighBit (x :: Nat) (n :: Nat) :: Bool where
  TestHighBit x n = ((2 ^ n) <=? x) -- x > 2^n

type ToBits x n = ToBits_ x n 'False
type family ToBits_ (x :: Nat) (n :: Nat) (started :: Bool) :: [Bool] where
  ToBits_ x 0 started = '[]
  ToBits_ x n started = ToBitsInner (TestHighBit x (n - 1)) x (n - 1) started
type family
  ToBitsInner (highBitSet :: Bool) (x :: Nat) (n :: Nat) (started :: Bool) :: [Bool] where
  ToBitsInner 'True  x n started = 'True  ': ToBits_ (x - 2^n) n 'True
  ToBitsInner 'False x n 'False  =           ToBits_ x         n 'False
  ToBitsInner 'False x n 'True   = 'False ': ToBits_ x         n 'True

type FromBits bits = FromBits_ bits 0
type family FromBits_ (bits :: [Bool]) (acc :: Nat) :: Nat where
  FromBits_ '[] acc = acc
  FromBits_ ('False ': rest) acc = FromBits_ rest (acc + acc)
  FromBits_ ('True  ': rest) acc = FromBits_ rest (1 + acc + acc)

type family
  ShiftBitsR (bits :: [Bool]) (n :: Nat) :: [Bool] where
  ShiftBitsR bits 0 = bits
  ShiftBitsR '[] n = '[]
  ShiftBitsR '[e] 1 = '[]
  ShiftBitsR (e ': rest) 1 = e ': ShiftBitsR rest 1
  ShiftBitsR (e ': rest) n = ShiftBitsR (ShiftBitsR (e ': rest) 1) (n - 1)

type family
  GetMostSignificantBitIndex (highestBit :: Nat) (n :: Nat) :: Nat where
  GetMostSignificantBitIndex          0 n = 1
  GetMostSignificantBitIndex highestBit n =
    If  (2 ^ (highestBit + 1) <=? n)
        (TypeError ('Text "number to big: "
                    ':<>: 'ShowType n
                    ':<>: 'Text " >= "
                    ':<>: 'ShowType (2 ^ (highestBit + 1))))
        (If (2 ^ highestBit <=? n)
            highestBit
            (GetMostSignificantBitIndex (highestBit - 1) n))

-- | Shift a type level natural to the right. This useful for division by powers
-- of two.
type family
  ShiftR (xMaxBits :: Nat) (x :: Nat) (bits :: Nat) :: Nat where
  ShiftR xMaxBits x n =
    FromBits
      (ShiftBitsR
        (ToBits x
                (1 + GetMostSignificantBitIndex xMaxBits x))
        n)
