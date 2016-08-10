{-# LANGUAGE UndecidableInstances #-}
-- | Type Level Pretty Printing
module Data.ByteString.IsoBaseFileFormat.Util.PrettyType where

import Data.Kind
import Data.Word
import Data.Type.Bool
import GHC.TypeLits
import Data.Bits
import Data.Proxy
import Text.Printf

-- | Mini Type level eDSL for pretty printing via 'PrettyTypeShow' (proxied) Use the type
-- aliases to get rid of unticked or even ticked promoteds.
data PrettyType where
  PrettyEmpty :: PrettyType
  PrettySpace :: PrettyType
  PrettyNewline :: PrettyType
  PrettySymbol :: PrettyPadded -> PrettyPrecision -> Symbol -> PrettyType
  PrettyNat :: PrettyPadded -> PrettyPrecision -> PrettyNatFormat -> Nat -> PrettyType
  TypePrettySeperatedBy :: PrettyType -> PrettyType -> PrettyType -> PrettyType

-- | Padding for 'PrettyType's 'PrettySymbol' and 'PrettyNat'.
data PrettyPadded where
  PrettyUnpadded :: PrettyPadded
  PrettyPadded
    :: Nat -- ^ Padding
    -> PrettyPadded

data PrettyPrecision where
  PrettyPrecise :: PrettyPrecision
  PrettyPrecision
    :: Nat -- ^ Precision
    -> PrettyPrecision

-- | 'Nat' formatting, e.g. "cafe", "CAFE", "51966" or "1100101011111110"
data PrettyNatFormat =
  PrettyHex | PrettyHexU | PrettyDec | PrettyBit

-- | A 'PrettyType' for a string
type PutStr str = 'PrettySymbol 'PrettyUnpadded 'PrettyPrecise str

-- | A 'PrettyType' for a string with a newline character at the end.
type PutStrLn str = PutStr str <++> PutStr "\n"

-- | A 'PrettyType' for a number.
type PutNat x = 'PrettyNat 'PrettyUnpadded 'PrettyPrecise 'PrettyDec x

-- | Concatenate two 'PrettyType'.
type (<++>) l r = 'TypePrettySeperatedBy 'PrettyEmpty l r
infixl 6 <++>

-- | Concatenate two 'PrettyType' using a 'PrettySpace'.
type (<+>) l r = 'TypePrettySeperatedBy 'PrettySpace l r
infixl 5 <+>

-- | Concatenate two 'PrettyType' using a 'PrettyNewline'.
type (<$$>) l r = 'TypePrettySeperatedBy 'PrettyNewline l r
infixl 4 <$$>

-- | Create 'PrettyType' from a 'Nat' formatted as 8 bit hex number
type PutHex8 x = 'PrettyNat 'PrettyUnpadded ('PrettyPrecision 2) 'PrettyHex x
-- | Create 'PrettyType' from a 'Nat' formatted as 16 bit hex number
type PutHex16 x = 'PrettyNat 'PrettyUnpadded ('PrettyPrecision 4) 'PrettyHex x
-- | Create 'PrettyType' from a 'Nat' formatted as 32 bit hex number
type PutHex32 x = 'PrettyNat 'PrettyUnpadded ('PrettyPrecision 8) 'PrettyHex x
-- | Create 'PrettyType' from a 'Nat' formatted as 64 bit hex number
type PutHex64 x = 'PrettyNat 'PrettyUnpadded ('PrettyPrecision 16) 'PrettyHex x

-- | Create 'PrettyType' from a 'Nat' formatted as 8-bit bit representation,
-- i.e. @5 => "00000101"@
type PutBits8 x = 'PrettyNat 'PrettyUnpadded ('PrettyPrecision 2) 'PrettyBit x
-- | Create 'PrettyType' from a 'Nat' formatted as 16-bit bit representation,
-- i.e. @5 => "00000000000000101"@
type PutBits16 x = 'PrettyNat 'PrettyUnpadded ('PrettyPrecision 4) 'PrettyBit x
-- | Create 'PrettyType' from a 'Nat' formatted as 32-bit bit representation,
-- i.e. @5 => "00000000000000000000000000000000101"@
type PutBits32 x = 'PrettyNat 'PrettyUnpadded ('PrettyPrecision 8) 'PrettyBit x
-- | Create 'PrettyType' from a 'Nat' formatted as 64-bit bit representation,
-- i.e. @5 => "00000000000000000000000000000000000000000000000000000000000000000000101"@
type PutBits64 x = 'PrettyNat 'PrettyUnpadded ('PrettyPrecision 16) 'PrettyBit x

-- | Combine a (type level) list of 'PrettyType's next to each other using
-- 'PrettySpace'
type family HSep (docs :: [PrettyType]) :: PrettyType where
  HSep '[] = 'PrettyEmpty
  HSep (t ': rest) = t <$$> (HSep rest)

-- | Combine a (type level) list of 'PrettyType's below each other using
-- 'PrettyNewline'
type family VSep (docs :: [PrettyType]) :: PrettyType where
  VSep '[] = 'PrettyEmpty
  VSep (t ': rest) = t <$$> (VSep rest)

-- | Repeat a 'PrettyType' @n@-times and append the copies.
type family PrettyOften (n :: Nat) (doc :: PrettyType) :: PrettyType where
  PrettyOften 0 doc = 'PrettyEmpty
  PrettyOften n doc = doc <++> PrettyOften (n-1) doc

-- PrettyTypeShow instances:

class PrettyTypeShow (p :: k) where
  ptShow :: proxy p -> String

instance PrettyTypeShow 'PrettyEmpty where ptShow _ = ""
instance PrettyTypeShow 'PrettySpace where ptShow _ = " "
instance PrettyTypeShow 'PrettyNewline where ptShow _ = "\n"

instance PrettyTypeShow 'PrettyUnpadded where ptShow _ = ""
instance forall p. KnownNat p => PrettyTypeShow ('PrettyPadded p) where
    ptShow _ = show (natVal (Proxy :: Proxy p))

instance PrettyTypeShow 'PrettyPrecise where ptShow _ = ""
instance forall p. KnownNat p => PrettyTypeShow ('PrettyPrecision p) where
    ptShow _ = "." ++ show (natVal (Proxy :: Proxy p))

instance forall t pad prec.
    (KnownSymbol t, PrettyTypeShow pad, PrettyTypeShow prec)
  => PrettyTypeShow ('PrettySymbol pad prec t) where
  ptShow _ = printf ("%" ++ ptShow (Proxy :: Proxy pad)
                         ++ ptShow (Proxy :: Proxy prec)
                         ++ "s")
                    (symbolVal (Proxy :: Proxy t))

instance forall fmt x pad prec.
  (KnownNat x, PrettyTypeShow fmt, PrettyTypeShow pad, PrettyTypeShow prec)
  => PrettyTypeShow ('PrettyNat pad prec fmt x) where
  ptShow _ = printf ("%" ++ ptShow (Proxy :: Proxy pad)
                         ++ ptShow (Proxy :: Proxy prec)
                         ++ ptShow (Proxy :: Proxy fmt))
                    (natVal (Proxy :: Proxy x))

-- Translation of 'PrettyNatFormat' to 'printf' format character:
instance PrettyTypeShow 'PrettyHex where ptShow _ = "x"
instance PrettyTypeShow 'PrettyHexU where ptShow _ = "X"
instance PrettyTypeShow 'PrettyDec where ptShow _ = "d"
instance PrettyTypeShow 'PrettyBit where ptShow _ = "b"

instance forall l r sep .
  (PrettyTypeShow sep, PrettyTypeShow l, PrettyTypeShow r)
  => PrettyTypeShow ('TypePrettySeperatedBy sep l r) where
  ptShow _ =
    let rstr = ptShow (Proxy :: Proxy r)
        lstr = ptShow (Proxy :: Proxy l)
        sepStr = ptShow (Proxy :: Proxy sep)
    in if lstr == "" then rstr
        else if rstr == "" then lstr
          else lstr ++ sepStr ++ rstr
