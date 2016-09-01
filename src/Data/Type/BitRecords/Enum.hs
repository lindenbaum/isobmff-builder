{-# LANGUAGE UndecidableInstances #-}
module Data.Type.BitRecords.Enum where

import Data.Type.BitRecords.Core
import Data.Type.BitRecords.Builder.Holey
import Data.Type.BitRecords.Builder.LazyByteStringBuilder
import Data.Proxy
import Data.Word
import GHC.TypeLits

-- * BitRecordFields containing /enum/-like types

-- | Return the numeric /index/ of an entry in a table. This emulates 'fromEnum' a bit.
type family FromEnum enum (entry :: enum) :: Nat

-- | Create a 'BitRecordField' for finite /enum-ish/ types.
--
-- @
-- data Color = Red | Green | Blue
--
-- type instance FromEnum Color 'Blue   = 3
-- type instance FromEnum Color 'Green  = 80
-- type instance FromEnum Color 'Red    = 12
--
-- type instance EnumFieldSize Color = 2
--
-- type ColorField = EnumField Color
--
-- type ErrorScreen = [utf8|Page fault ;)|] :>: ColorField := 'Blue
--
-- @
--
-- Now we can use @ErrorScreen@ as 'BitRecord':
--
-- >>> bitStringPrinter (Proxy @ErrorScreen)
-- "<< 50 61 67 65 20 66 61 75 6c 74 20 3b 29 c0 >>"
--
-- The 2-bit color field is set to @3@ and is the end: @0c@
--
-- To pass a runtime value just omit the assignment, e.g. @ := 'Blue@ in the
-- other example, and the 'Holey' magic will return a function of 'EnumValue':
--
-- >>> :t bitStringPrinter (Proxy @(ToBitRecord (EnumField Color)))
-- bitStringPrinter (Proxy @(ToBitRecord (EnumField Color)))
--   :: EnumValue Color -> [Char]
--
-- To create an 'EnumValue' one has to use a proxy:
--
-- >>> bitStringPrinter (Proxy @(ToBitRecord (EnumField Color))) (EnumValue (Proxy @'Blue))
-- "<< c0 >>"
--
type family EnumField enum :: BitRecordField where
  EnumField enum = 'MkField (EnumValue enum) (EnumFieldSize enum)

type family EnumFieldSize enum :: Nat

data EnumValue e where
  EnumValue :: KnownNat (FromEnum e v) => Proxy (v :: e) -> EnumValue e

fromEnumValue :: EnumValue e -> Word64
fromEnumValue (EnumValue p) = enumValue p
  where
    enumValue :: forall proxy (v :: enum) . KnownNat (FromEnum enum v) => proxy v -> Word64
    enumValue _ = fromIntegral (natVal (Proxy @(FromEnum enum v)))

instance
  forall s r e .
  BitStringBuilderHoley (Proxy ('MkField Word64 s)) r =>
  BitStringBuilderHoley (Proxy ('MkField (EnumValue e) s)) r
  where
    type ToBitStringBuilder (Proxy ('MkField (EnumValue e) s)) r =
      EnumValue e -> r
    bitStringBuilderHoley _ =
      mapHoley ( . fromEnumValue)
               (bitStringBuilderHoley (Proxy @('MkField Word64 s)))

instance
  forall e (v :: e) s r .
  (KnownNat (FromEnum e v)
  ,BitStringBuilderHoley (Proxy ('MkField (EnumValue e) s)) r) =>
  BitStringBuilderHoley (Proxy ('AssignF v ('MkField (EnumValue e) s))) r
  where
    bitStringBuilderHoley _ =
      applyHoley
         (bitStringBuilderHoley (Proxy @('MkField (EnumValue e) s)))
         (EnumValue (Proxy @v))
