
{-# LANGUAGE UndecidableInstances #-}
-- | Meta data for a presentation of a /movie/.
module Data.ByteString.IsoBaseFileFormat.Boxes.MovieBox where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box
import Data.Kind  (Type, Constraint)
import Data.Int
import Data.Type.Equality
-- import Data.Type.List
import Data.Vector.Sized
import Data.Singletons
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

data TemplateField (v :: k) o where
  DefaultField :: SingI val =>            TemplateField val outType
  SetField     :: SingI val => outType -> TemplateField val outType

class (IsBoxContent f) => IsField f where
  type SField f 
  fromTypeVal :: forall (st :: (SField f)) . (SingKind (SField f))  => Sing st -> f

instance IsField (Field Word8) where
  type SField (Field Word8) = Nat
  fromTypeVal = Field . fromIntegral . fromSing

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
