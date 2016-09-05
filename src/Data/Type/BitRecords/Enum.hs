{-# LANGUAGE UndecidableInstances #-}
module Data.Type.BitRecords.Enum where

import Data.Type.BitRecords.Core
import Data.Type.BitRecords.Builder.Holey
import Data.Type.BitRecords.Builder.LazyByteStringBuilder
import Data.Proxy
import Data.Word
import GHC.TypeLits
import Data.Kind.Extra

-- * BitRecordFields containing /enum/-like types

type instance ToBitRecord (ef :: IsAn (EnumField e s)) =
  ef --> EnumOf e --> BitRecordOf (EnumOf e) -->| BitRecord

data EnumOf enum where
  MkEnumOf :: BitRecord -> EnumOf enum

type instance ToBitRecord (t :: EnumOf e) =
  t ~~> BitRecordOf (EnumOf e) -->| BitRecord

type instance Eval (('MkEnumOf br :: EnumOf e) ~~> BitRecordOf (EnumOf e)) =
  'MkBitRecord br

data EnumField enum (size :: Nat) where
  MkEnumField :: BitRecord -> EnumField e s

type instance Eval (('MkEnumField br :: EnumField e s) ~~> EnumOf e) =
  'MkEnumOf br

-- | Create a 'BitRecord' for setting an enum to a value.
type family StaticEnumRecord (ef :: IsAn (EnumField enum size)) (v :: enum) :: BitRecord where
  StaticEnumRecord ef v =
    ToBitRecord (SetTo ef v)

-- | Create an 'EnumOf' that sets an enum to a static value.
type SetEnum (ef :: IsAn (EnumField enum size)) (v :: enum) =
  (SetTo ef v -->| EnumOf enum)

-- | Create an 'EnumOf' that sets the enum to a runtime value.
type EnumParam (ef :: IsAn (EnumField enum size)) (label :: Symbol) =
  (Defer label ef -->| EnumOf enum)

-- | Create an 'EnumOf' that sets an extended enum to an extended static value.
type SetEnumAlt (ef :: IsAn (EnumField enum size)) v =
  (SetToAlt ef v -->| EnumOf enum)

-- | Create an 'EnumOf' that sets the extended enum to a runtime value.
type EnumParamAlt (ef :: IsAn (EnumField enum size)) (label :: Symbol) =
  (DeferAlt label ef -->| EnumOf enum)

-- ** Composing BitRecords with enum fields

-- | A fixed size 'EnumField'
data FixedEnum enum size :: IsAn (EnumField enum size)

-- | An enum that can be extended with an additional 'BitRecordField', following
-- the  regular enum field; the extension is optional, i.e. only if the
-- /regular/  field contains a special value (e.g. 0xff).
data ExtEnum enum size (extInd :: enum) extField :: IsAn (EnumField enum size)

-- | Return the numeric /index/ of an entry in a table. This emulates 'fromEnum' a bit.
type family FromEnum enum (entry :: enum) :: Nat

type instance Eval (SetWith (ei :: IsAn (EnumField enum size)) (OverwriteWith value)) =
  'MkEnumField ('BitRecordMember (Field size := FromEnum enum value))

type instance Eval (SetWith (ei :: IsAn (EnumField enum size)) (NamedRuntimeParameter label)) =
  'MkEnumField ('BitRecordMember (label :=> 'MkField (EnumValue enum) size))

type instance Eval (SetWith (ExtEnum enum size extInd extField) (AltSetter (OverwriteWith value))) =
  'MkEnumField (Field size := FromEnum enum extInd :>: extField := value)
type instance Eval (SetWith (FixedEnum enum size) (AltSetter (OverwriteWith value))) =
  TypeError ('Text "Cannot assign an extension value to the FixedEnum " ':<>: 'ShowType enum)

type instance Eval (SetWith (ExtEnum enum size extId extField) (AltSetter (NamedRuntimeParameter label))) =
  'MkEnumField ('BitRecordMember (label :=> 'MkField (EnumValue enum) size))
type instance Eval (SetWith (FixedEnum enum size) (AltSetter (NamedRuntimeParameter label))) =
  TypeError ('Text "Cannot assign an extension value to the FixedEnum " ':<>: 'ShowType enum)

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
