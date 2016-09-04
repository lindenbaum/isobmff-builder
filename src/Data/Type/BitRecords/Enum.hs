{-# LANGUAGE UndecidableInstances #-}
module Data.Type.BitRecords.Enum where

import Data.Type.BitRecords.Core
import Data.Type.BitRecords.Builder.Holey
import Data.Type.BitRecords.Builder.LazyByteStringBuilder
import Data.Proxy
import Data.Word
import GHC.TypeLits
import Data.Kind (type Type)

-- * BitRecordFields containing /enum/-like types

-- | Create a 'BitRecordField' for finite /enum-ish/ types.
data EnumInfo :: Type -> Nat -> Type

-- | Return the numeric /index/ of an entry in a table. This emulates 'fromEnum' a bit.
type family FromEnum enum (entry :: enum) :: Nat

type ToEnumInfo enum size = EnumInfo enum size -> Type

-- | An enum that has no extension fields.
data FixedEnum enum size :: ToEnumInfo enum size

-- | An enum that can be extended with an additional 'BitRecordField', following
-- the  regular enum field; the extension is optional, i.e. only if the
-- /regular/  field contains a special value (e.g. 0xff).
data ExtEnum enum size (extInd :: enum) extField :: ToEnumInfo enum size

data EnumOf enum
type EnumRecordFor enum = IsA (BitRecordOf (EnumOf enum))

data SetEnumTo :: ToEnumInfo enum n -> enum -> EnumRecordFor enum
type instance Eval (SetEnumTo (ei :: ToEnumInfo enum size) value) =
  'MkBitRecord (ToBitRecord (Field size := FromEnum enum value))

data SetEnumToExt :: ToEnumInfo enum size -> extValue -> EnumRecordFor enum
type instance Eval (SetEnumToExt (ExtEnum enum size extInd extField) extValue) =
  'MkBitRecord (Field size := FromEnum enum extInd :>: extField := extValue)
type instance Eval (SetEnumToExt (FixedEnum enum size) extValue) =
  TypeError ('Text "Cannot extend fixed-enum: " ':<>: 'ShowType enum)

data DeferEnum :: Symbol -> ToEnumInfo enum n -> EnumRecordFor enum
type instance Eval (DeferEnum label (ie :: ToEnumInfo enum size)) =
  'MkBitRecord (ToBitRecord (label :=> 'MkField (EnumValue enum) size))

data DeferEnumExt :: Symbol -> ToEnumInfo enum n -> EnumRecordFor enum
type instance Eval (DeferEnumExt label (ExtEnum enum size extInd extField)) =
   'MkBitRecord (Field size := FromEnum enum extInd :>: label :=> extField)
type instance Eval (DeferEnumExt label (FixedEnum enum size)) =
  TypeError ('Text "Cannot extend fixed-enum: " ':<>: 'ShowType enum)

data EnumValue e where
  EnumValue :: KnownNat (FromEnum e v) => Proxy (v :: e) -> EnumValue e

fromEnumValue :: EnumValue e -> Word64
fromEnumValue (EnumValue p) = enumValue p
  where
    enumValue :: forall proxy (v :: enum) . KnownNat (FromEnum enum v) => proxy v -> Word64
    enumValue _ = fromIntegral (natVal (Proxy @(FromEnum enum v)))


instance
  forall size r e .
  BitStringBuilderHoley (Proxy ('MkField Word64 size)) r =>
  BitStringBuilderHoley (Proxy ('MkField (EnumValue e) size)) r
  where
    type ToBitStringBuilder (Proxy ('MkField (EnumValue e) size)) r =
      EnumValue e -> r
    bitStringBuilderHoley _ =
      mapHoley ( . fromEnumValue)
               (bitStringBuilderHoley (Proxy @('MkField Word64 size)))
