{-# LANGUAGE UndecidableInstances #-}
module Data.Type.BitRecords.Enum where

import Data.Type.BitRecords.Core
import Data.Type.BitRecords.Builder.BitBuffer
import Data.Type.BitRecords.Builder.Holey
import Data.Type.BitRecords.Builder.LazyByteStringBuilder
import Data.Proxy
import Data.Word
import Data.Kind (Type)
import GHC.TypeLits
import Data.Kind.Extra
import Data.Type.Pretty

-- * BitRecordFields containing /enum/-like types

-- | Wrapper around a type that can be represented as a short number, indexing
-- the clauses of the (sum) type.
data EnumOf enum where
  MkEnumOf
    :: IsAn (EnumField enum size)
    -> IsA (FieldValue label enum)
    -> BitRecord
    -> EnumOf enum

type BitRecordOfEnum (e :: IsAn (EnumOf enum)) = (RenderEnumOf (Eval e) :: BitRecord)

type family RenderEnumOf (e :: EnumOf enum) :: BitRecord where
  RenderEnumOf ('MkEnumOf mainField mainFieldVal extra) =
    (BitRecordFieldOfEnumField mainField) :~ mainFieldVal .+: extra

-- | Physical representation of an 'EnumOf', this is an abstract type
data EnumField (enum :: Type) (size :: Nat)

type BitRecordFieldOfEnumField (x :: IsA (EnumField e s)) =
  MkField ('MkFieldCustom :: BitField (EnumValue e) e s)

-- | A fixed size 'EnumField'
data FixedEnum (enum :: Type) (size :: Nat) :: IsAn (EnumField enum size)

-- | An enum that can be extended with an additional 'BitRecordField', following
-- the  regular enum field; the extension is optional, i.e. only if the
-- /regular/  field contains a special value (e.g. 0xff).
data ExtEnum (enum :: Type)
             (size :: Nat)
             (extInd :: enum)
             (extField :: IsA (BitRecordField (t :: BitField rt0 (st0 :: k0) len0)))
             :: IsAn (EnumField enum size)

-- | Create an 'EnumOf' that sets an enum to a static value.
data SetEnum (l :: Symbol) (ef :: IsAn (EnumField enum size)) (v :: enum) :: IsAn (EnumOf enum)

type instance Eval (SetEnum (l :: Symbol) (ei :: IsAn (EnumField enum size)) value) =
  'MkEnumOf
     ei
     (StaticFieldValue l value)
     'EmptyBitRecord

-- | Create an 'EnumOf' that sets the enum to a runtime value.
data EnumParam
     (label :: Symbol)
     (ef :: IsAn (EnumField (enum :: Type) (size :: Nat)))
     :: IsAn (EnumOf enum)
type instance Eval (EnumParam label (ei :: IsAn (EnumField enum size))) =
  'MkEnumOf
     ei
     (RuntimeFieldValue label)
     'EmptyBitRecord

-- | Create an 'EnumOf' that sets an extended enum to an extended static value.
data SetEnumAlt (l :: Symbol) (ef :: IsAn (EnumField (enum :: Type) (size :: Nat))) (v :: k)
  :: IsAn (EnumOf enum)

type instance Eval (SetEnumAlt (l :: Symbol) (ExtEnum enum size extInd extField) value) =
  -- TODO maybe enrich the demoteRep type of 'MkField??
  'MkEnumOf
     (ExtEnum enum size extInd extField)
     (StaticFieldValue l extInd)
     ('BitRecordMember (extField := value))

type instance Eval (SetEnumAlt (l :: Symbol) (FixedEnum enum size) value) =
  TypeError ('Text "Cannot assign an 'extended' value to the 'FixedEnum' "
             ':<>: 'ShowType enum)

-- | Create an 'EnumOf' that sets the extended enum to a runtime value.
data EnumParamAlt
  (label :: Symbol)
  (ef :: IsAn (EnumField (enum :: Type) (size :: Nat)))
  :: IsAn (EnumOf enum)

type instance Eval (EnumParamAlt label (ExtEnum enum size extInd extField)) =
  'MkEnumOf
  (ExtEnum enum size extInd extField)
  (StaticFieldValue label extInd)
  ('BitRecordMember (extField :~ RuntimeFieldValue label))

type instance Eval (EnumParamAlt label (FixedEnum enum size)) =
  TypeError ('Text "Cannot assign an extension value to the FixedEnum "
             ':<>: 'ShowType enum)

-- ** Composing BitRecords with enum fields

-- | Return the numeric /index/ of an entry in a table. This emulates 'fromEnum' a bit.
type family FromEnum enum (entry :: enum) :: Nat

-- | An enum value supplied at runtime.
data EnumValue e where
  MkEnumValue :: KnownNat (FromEnum e v) => Proxy (v :: e) -> EnumValue e

-- | Create an 'EnumValue' from a 'Proxy'. TODO remove?
enumValueProxy :: KnownNat (FromEnum e v) => Proxy (v :: e) -> EnumValue e
enumValueProxy = MkEnumValue

fromEnumValue :: EnumValue e -> Word64
fromEnumValue (MkEnumValue p) = enumValue p
  where
    enumValue :: forall proxy (v :: enum) . KnownNat (FromEnum enum v) => proxy v -> Word64
    enumValue _ = fromIntegral (natVal (Proxy @(FromEnum enum v)))

instance
  forall (size :: Nat) r e (v :: e) (f :: IsA (BitRecordField ('MkFieldCustom :: BitField (EnumValue e) e size))) .
    (KnownNat (FromEnum e v), KnownChunkSize size) =>
  BitStringBuilderHoley (Proxy (f := v)) r where
  bitStringBuilderHoley _ =
    immediate (appendBitString
               (bitStringProxyLength (Proxy @size)
                 (fromIntegral (natVal (Proxy @(FromEnum e v))))))

instance
  forall (size :: Nat) r e  .
  (KnownChunkSize size) =>
  BitStringBuilderHoley (Proxy (MkField ('MkFieldCustom :: BitField (EnumValue e) e size))) r
  where
    type ToBitStringBuilder (Proxy (MkField ('MkFieldCustom :: BitField (EnumValue e) e size))) r =
      EnumValue e -> r
    bitStringBuilderHoley _ =
        indirect (appendBitString . bitStringProxyLength (Proxy @size) . fromEnumValue)

type instance ToPretty (EnumValue e) = PutStr "<<enum>>"
type instance PrettyCustomFieldValue (EnumValue e) e size (v :: e) =
  PutNat (FromEnum e v) <+> ("hex" <:> PutHex (FromEnum e v)) <+> ("bin" <:> PutBits (FromEnum e v))
