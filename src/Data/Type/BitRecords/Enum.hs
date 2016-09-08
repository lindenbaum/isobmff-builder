{-# LANGUAGE UndecidableInstances #-}
module Data.Type.BitRecords.Enum where

import Data.Type.BitRecords.Core
import Data.Type.BitRecords.Builder.Holey
import Data.Type.BitRecords.Builder.LazyByteStringBuilder
import Data.Proxy
import Data.Word
import Data.Kind (Type)
import GHC.TypeLits
import Data.Kind.Extra

-- * BitRecordFields containing /enum/-like types

-- | Wrapper around a type that can be represented as a short number, indexing
-- the clauses of the (sum) type.
data EnumOf enum where
  MkEnumOf :: IsA BitRecord -> EnumOf enum

type instance ('MkEnumOf b :: EnumOf e) ~~> BitRecord = Eval b

-- | Physical representation of an 'EnumOf', this is an abstract type
data EnumField (enum :: Type) (size :: Nat)

-- | A fixed size 'EnumField'
data FixedEnum (enum :: Type) (size :: Nat) :: IsAn (EnumField enum size)

-- | An enum that can be extended with an additional 'BitRecordField', following
-- the  regular enum field; the extension is optional, i.e. only if the
-- /regular/  field contains a special value (e.g. 0xff).
data ExtEnum (enum :: Type) (size :: Nat) (extInd :: enum) (extField :: IsA BitRecordField) :: IsAn (EnumField enum size)

-- | Create an 'EnumOf' that sets an enum to a static value.
data SetEnum (ef :: IsAn (EnumField enum size)) (v :: enum) :: IsAn (EnumOf enum)

type instance Eval (SetEnum (ei :: IsAn (EnumField enum size)) value) =
  'MkEnumOf (Itself ('BitRecordMember (Eval (Field size) := FromEnum enum value)))

-- | Create an 'EnumOf' that sets the enum to a runtime value.
data EnumParam (ef :: IsAn (EnumField (enum :: Type) (size :: Nat))) (label :: Symbol) :: IsAn (EnumOf enum)
type instance Eval (EnumParam (ei :: IsAn (EnumField enum size)) label) =
  'MkEnumOf (Itself ('BitRecordMember (label @: 'MkField (EnumValue enum) size)))

-- | Create an 'EnumOf' that sets an extended enum to an extended static value.
data SetEnumAlt (ef :: IsAn (EnumField (enum :: Type) (size :: Nat))) (v :: k) :: IsAn (EnumOf enum)

type instance Eval (SetEnumAlt (ExtEnum enum size extInd extField) value) =
  -- TODO maybe enrich the demoteRep type of 'MkField??
  'MkEnumOf
  (Eval (Field size) := FromEnum enum extInd .>. Eval extField := value)

type instance Eval (SetEnumAlt (FixedEnum enum size) value) =
  TypeError ('Text "Cannot assign an 'extended' value to the 'FixedEnum' "
             ':<>: 'ShowType enum)

-- | Create an 'EnumOf' that sets the extended enum to a runtime value.
data EnumParamAlt
  (ef :: IsAn (EnumField (enum :: Type) (size :: Nat)))
  (label :: Symbol)
  :: IsAn (EnumOf enum)

type instance Eval (EnumParamAlt (ExtEnum enum size extId extField) label) =
  'MkEnumOf (Itself ('BitRecordMember (label @: 'MkField (EnumValue enum) size)))

type instance Eval (EnumParamAlt (FixedEnum enum size) label) =
  TypeError ('Text "Cannot assign an extension value to the FixedEnum "
             ':<>: 'ShowType enum)

-- ** Composing BitRecords with enum fields

-- | Return the numeric /index/ of an entry in a table. This emulates 'fromEnum' a bit.
type family FromEnum enum (entry :: enum) :: Nat

-- | An enum value supplied at runtime.
data EnumValue e where
  MkEnumValue :: KnownNat (FromEnum e v) => Proxy (v :: e) -> EnumValue e

fromEnumValue :: EnumValue e -> Word64
fromEnumValue (MkEnumValue p) = enumValue p
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
