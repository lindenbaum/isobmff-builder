
{-# LANGUAGE UndecidableInstances #-}
-- | Meta data for a presentation of a /movie/.
module Data.ByteString.IsoBaseFileFormat.Boxes.MovieBox where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box
import Data.Kind  (Type, Constraint)
import Data.Int
import Data.Type.Equality
import Data.Type.List
import Data.Vector.Sized
import Data.Singletons.Prelude.List

-- | The metadata for a presentation, a single 'MovieBox' which occurs only once
-- and top-level. It is pretty empty on it's own, but it contains nested boxes
-- with all the relevant meta data.
type MovieBox = Box "moov"

instance BoxRules "moov" where
  type RequiredNestedBoxes "moov" = '["mvhd", "trak"]

-- | A movie parent box. Combine with the nested 'Boxes' using 'boxes' or '(^-)'
--
-- Example:
-- >  xxx :: Box "moov"
-- >  xxx = movieBox
-- >         ^- Nested (movieHeaderBox (...))
-- >                   :- (trackBox
-- >                       ^- Nested (trackHeaderBox (TrackHeader ...))
-- >                                 :- trackReferenceBox (TrackReference ...)
-- >                                 :- trackGroupingIndication (TrackGroupingInd ...))
--
movieBox :: ParentBox "moov"
movieBox = emptyParentBox

-- * @mvhd@ Box

newtype Field scalarType = Field scalarType

instance IsBoxContent (Field Word8) where
  boxSize _ = 1
  boxBuilder (Field v) = word8 v

instance IsBoxContent (Field Word16) where
  boxSize _ = 2
  boxBuilder (Field v) = word16BE v

instance IsBoxContent (Field Word32) where
  boxSize _ = 4
  boxBuilder (Field v) = word32BE v

instance IsBoxContent (Field Word64) where
  boxSize _ = 8
  boxBuilder (Field v) = word64BE v

instance IsBoxContent (Field Int8) where
  boxSize _ = 1
  boxBuilder (Field v) = int8 v

instance IsBoxContent (Field Int16) where
  boxSize _ = 2
  boxBuilder (Field v) = int16BE v

instance IsBoxContent (Field Int32) where
  boxSize _ = 4
  boxBuilder (Field v) = int32BE v

instance IsBoxContent (Field Int64) where
  boxSize _ = 8
  boxBuilder (Field v) = int64BE v


-- * Field with (type-level) overidable predefined default values

data TemplateField (defaultValue :: k) c where
  DefaultField ::      TemplateField defaultValue t
  SetField     :: t -> TemplateField defaultValue t

instance ( IsFieldValue v
         , FieldVal v ~ Integer
         , Num t
         , IsBoxContent (Field t))
         => IsBoxContent (TemplateField v t) where
  boxSize DefaultField = boxSize (Field ((fromIntegral (fieldVal (Proxy :: Proxy v))) :: t))
  boxSize (SetField t) = boxSize (Field t) -- TODO should always be the same??
  boxBuilder DefaultField = boxBuilder (Field ((fromIntegral (fieldVal (Proxy :: Proxy v))) :: t))
  boxBuilder (SetField t) = boxBuilder (Field t)

class IsFieldValue (f :: t) where
  type FieldVal f
  fieldVal :: proxy f -> FieldVal f

class IsBoxContent f => IsField f tf where
  type TypeValConstraint t tf (x :: tf) :: Constraint
  type TypeValConstraint t tf x = ()
  fromTypeVal :: forall proxy (x :: tf) . TypeValConstraint f tf x => proxy x -> f


instance (Num a, IsBoxContent (Field a)) => IsField (Field a) Nat where
  type TypeValConstraint (Field a) Nat x = KnownNat x
  fromTypeVal = Field . fromIntegral . natVal

instance (IsBoxContent (Vector 0 a), IsField a e) => IsField (Vector 0 a) [e] where
  type TypeValConstraint (Vector size a) [e] '[] = ()
  fromTypeVal _ = empty


instance ( IsBoxContent (Vector incSize a)
         , IsField (Vector size a) [e]
         , incSize ~ (size + 1))
         => IsField (Vector incSize a) [e] where
  type TypeValConstraint (Vector incSize a) [e] (x ': xs) =
    (
      TypeValConstraint a e x
    , TypeValConstraint (Vector (incSize - 1) a) [e] xs
    )
  fromTypeVal tlistPx =
    snoc (fromTypeVal (Proxy :: Proxy xs) :: Vector size a)
         (fromTypeVal (Proxy :: Proxy x))


type family MapC (xs :: [k]) (toConstraint :: k -> Constraint) :: Constraint where
  MapC '[] toConstraint = ()
  MapC (e ': rest) toConstraint = (toConstraint e, MapC rest toConstraint)

instance KnownNat f => IsFieldValue (f :: Nat) where
  type FieldVal f = Integer
  fieldVal = natVal

instance IsFieldValue (TemplateField '[] (t :: Type)) where
  type FieldVal (TemplateField '[] t) = Vector 0 t
  fieldVal _                       = empty

instance ( IsFieldValue                              (t :: k)
         , rt                                      ~ FieldVal t
         , IsFieldValue                              (TemplateField (ts :: [k]) rt)
         , FieldVal (TemplateField (ts :: [k]) rt) ~ Vector (Length ts) rt
         )
      => IsFieldValue (TemplateField (t ': ts) rt) where
  type FieldVal (TemplateField (t ': ts) rt) =
    Vector (Length ts + 1) rt
  fieldVal _ =
    cons (fieldVal (Proxy :: Proxy t))
         (fieldVal (Proxy :: Proxy (TemplateField ts rt)))


instance (Num t, IsBoxContent (Field t), KnownNat v) => IsBoxContent (TemplateField v t) where
  boxSize _ = boxSize (Field (fromIntegral (natVal (Proxy :: Proxy v))) :: Field t)
  boxBuilder DefaultField = boxBuilder (Field (fromIntegral (natVal (Proxy :: Proxy v))) :: Field t)
  boxBuilder (SetField v) = boxBuilder (Field v :: Field t)

-- * Field with (type-level) predefined default values


-- | Overall media independent inforation, concerning the complete movie.
type MovieHeader =
     TemplateField 0x00010000 Word32
  :+ Field Word8
  :+ TemplateField  '[0x00010000,0,0,0,0x00010000,0,0,0,0x40000000] Int32
-- xxx :: MovieHeader
-- xxx = DefaultField <+> Field 100 <+> DefaultField

-- zzz = boxBuilder xxx

 -- {mvhdTiming :: MovieTiming time
 -- ,mvhdRate :: TemplateField 0x00010000 Word32
--   ,mvhdVolume :: TemplateField 0x0100 Word16
--   ,mvhdReserved1 :: TemplateField 0 Word16
--   ,mvhdReserved2 :: ConstVectorField 0 Word32
--   ,mvhdMatrix :: TemplateVectorField '[0x00010000,0,0,0,0x00010000,0,0,0,0x40000000] Int32
--   ,mvhdPreDefined :: TemplateVectorField '[0,0,0,0,0,0] Word32
--   ,mvhdNextTrackId :: Field Word32
-- }

data MovieTiming time =
  MovieTiming {creationTime :: Field time
              ,modificationTime :: Field time
              ,timescale :: Field time
              ,duration :: Field time
              }

instance BoxRules "mvhd" where
  type IsTopLevelBox "mvhd" = 'False
  type GetCardinality "mvhd" any = 'ExactlyOnce
