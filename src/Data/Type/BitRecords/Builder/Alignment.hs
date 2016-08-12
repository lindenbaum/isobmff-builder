{-# LANGUAGE UndecidableInstances #-}
module Data.Type.BitRecords.Builder.Alignment where

import Data.Type.BitRecords.Arithmetic
import Data.Bits
import Data.Proxy
import Data.Word
import Data.Type.Equality
import Data.Type.Bool
import GHC.TypeLits
import Data.Int
import Text.Printf
import Data.Kind hiding (type (*))
import Prelude hiding ((.), id)

-- * Endianness

-- | The endianness for the serialization of a 'BitBuffer'.
data Endianness = BigEndian | LittleEndian

-- | A 'Proxy' for the promoted type 'BigEndian', e.g. for
-- 'formatAlignedBits'.
bigEndian :: Proxy 'BigEndian
bigEndian = Proxy

-- | A 'Proxy' for the promoted type 'LittleEndian', e.g. for
-- 'formatAlignedBits'.
littleEndian :: Proxy 'LittleEndian
littleEndian = Proxy

-- * Alignment

-- | Alignments for optimized writing of bits into bytestrings.
data Alignment = Align8 | Align16 | Align32 | Align64

-- | A convenience contraint type alias for 'Alignment's.
type KnownAlignment a =
  ( Num (ToAlignedWord a)
  , Show (ToAlignedWord a)
  , Bits (ToAlignedWord a)
  , FiniteBits (ToAlignedWord a)
  , Ord (ToAlignedWord a)
  , Eq (ToAlignedWord a)
  , PrintfArg (ToAlignedWord a)
  , KnownNat (GetAlignmentBits a))

-- | Constructor for a proxy fot the promoted 'Align8', e.g. for
-- 'formatBits'.
align8 :: Proxy 'Align8
align8 = Proxy

-- | Constructor for a proxy fot the promoted 'Align16', e.g. for
-- 'formatBits'.
align16 :: Proxy 'Align16
align16 = Proxy

-- | Constructor for a proxy fot the promoted 'Align32', e.g. for
-- 'formatBits'.
align32 :: Proxy 'Align32
align32 = Proxy

-- | Constructor for a proxy fot the promoted 'Align64', e.g. for
-- 'formatBits'.
align64 :: Proxy 'Align64
align64 = Proxy

type family
  AlignmentOffsetAdd (a :: Alignment) (len :: Nat) (offset :: Nat) :: Nat where
  AlignmentOffsetAdd a len offset = AlignmentOffset a (len + offset)

type family AlignmentOffset (a :: Alignment) (x :: Nat) :: Nat where
  AlignmentOffset 'Align64 x = x `RemPow2` 6
  AlignmentOffset 'Align32 x = x `RemPow2` 5
  AlignmentOffset 'Align16 x = x `RemPow2` 4
  AlignmentOffset 'Align8 x = x `RemPow2` 3

-- | Return an adequate alignment for records with @n@ bits.
type family SelectAlignment (n :: Nat) :: Maybe Alignment where
  SelectAlignment  x =
     If ((x `RemPow2` 6) == 0) ('Just 'Align64)
    (If ((x `RemPow2` 5) == 0) ('Just 'Align32)
    (If ((x `RemPow2` 4) == 0) ('Just 'Align16)
    (If ((x `RemPow2` 3) == 0) ('Just 'Align8)
       (TypeError ('Text "Bit record size is not divisable by 8: "
                   ':<>: 'ShowType x)))))

type family GetAlignmentBits (a :: Alignment) :: Nat where
  GetAlignmentBits 'Align8 = 8
  GetAlignmentBits 'Align16 = 16
  GetAlignmentBits 'Align32 = 32
  GetAlignmentBits 'Align64 = 64

-- | Calculate the number of unaligned bits with respect to a specific
-- 'Alignment'. This is just the type level integer remainder using the number
-- of bits in the alignment as returned by 'GetAlignmentBits'.
type family GetRemainingUnaligned (n :: Nat) (a :: Alignment) :: Nat where
  GetRemainingUnaligned n a = n `Rem` GetAlignmentBits a

type family MustFitInto (a :: Alignment) k :: Constraint where
  MustFitInto a t =
    If (FitsInto a t)
      ( 'True ~ 'True )
      (TypeError
        ('Text "Cannot fit " ':<>:
         'ShowType t ':<>:
         'Text " into " ':<>:
         'ShowType a))

type family FitsInto (a :: Alignment) k :: Bool where
  FitsInto 'Align64 Word64 = 'True
  FitsInto 'Align64 Word32 = 'True
  FitsInto 'Align64 Word16 = 'True
  FitsInto 'Align64 Word8 = 'True
  FitsInto 'Align64 Int64 = 'True
  FitsInto 'Align64 Int32 = 'True
  FitsInto 'Align64 Int16 = 'True
  FitsInto 'Align64 Int8 = 'True
  FitsInto 'Align64 _    = 'False
  FitsInto 'Align32 Word32 = 'True
  FitsInto 'Align32 Word16 = 'True
  FitsInto 'Align32 Word8 = 'True
  FitsInto 'Align32 Int32 = 'True
  FitsInto 'Align32 Int16 = 'True
  FitsInto 'Align32 Int8 = 'True
  FitsInto 'Align32 _    = 'False
  FitsInto 'Align16 Word16 = 'True
  FitsInto 'Align16 Word8 = 'True
  FitsInto 'Align16 Int16 = 'True
  FitsInto 'Align16 Int8 = 'True
  FitsInto 'Align16 _    = 'False
  FitsInto 'Align8 Word8 = 'True
  FitsInto 'Align8 Int8 = 'True
  FitsInto 'Align8 _    = 'False

type family ToAlignedWord (alignemnt :: Alignment) where
  ToAlignedWord 'Align64 = Word64
  ToAlignedWord 'Align32 = Word32
  ToAlignedWord 'Align16 = Word16
  ToAlignedWord 'Align8  =  Word8
