{-# LANGUAGE UndecidableInstances #-}
-- | Connect a sample description to a data reference via an index into the data
-- reference entry table.
module Data.ByteString.IsoBaseFileFormat.Boxes.SampleEntry where

import Data.ByteString.IsoBaseFileFormat.Box
import Data.ByteString.IsoBaseFileFormat.Boxes.Handler
import Data.ByteString.IsoBaseFileFormat.Util.BoxFields
import Data.ByteString.IsoBaseFileFormat.ReExports

-- | Specific sample entries must provide an instance for this data family.
-- The @format@ parameter will be used as 'BoxTypeSymbol'.
data family SampleEntry (handlertype :: HandlerType) (format :: k)

-- | Create a 'SampleEntry' 'Box' from the data reference index and the
-- 'HandlerType' specific 'SampleEntry' instance.
sampleEntry
  :: forall (handlertype :: HandlerType) (format :: k)
  .  U16 "data_reference_index"
  -> SampleEntry handlertype format
  -> Box (SampleEntry handlertype format)
sampleEntry i c = Box (SampleEntryFields (Constant :+ i :+ c))

-- | A common header for all specific sample entries, the 'BoxContent' of the
-- abstract 'SampleEntry' is @SampleEntryFields (SampleEntry h f)@.
newtype SampleEntryFields a where
  SampleEntryFields
    :: Constant (U8Arr "reserved" 6) '[0,0,0,0,0,0]
    :+ U16 "data_reference_index"
    :+ a
    -> SampleEntryFields a
deriving instance IsBoxContent a => IsBoxContent (SampleEntryFields a)
deriving instance Default a => Default (SampleEntryFields a)

-- | Use this in 'IsMediaFileFormat's 'BoxLayout' to range over any specific
-- 'SampleEntry', disregarding the second parameter (that indicates low-level
-- format, protocol or codec characteristics).
data MatchSampleEntry (handlerType :: HandlerType)

type instance
  IsRuleConform (Box (SampleEntry g' f)) (MatchSampleEntry g) =
    HandlerTypeCode g' == HandlerTypeCode g

-- | The 'BoxTypeSymbol' of sample entry is exactly the @format@ type index of
-- the 'SampleEntry' family.
type instance
  BoxTypeSymbol (SampleEntry handlertype format) = BoxTypeSymbol format

instance (IsBoxContent (SampleEntry handlertype format)
         ,KnownSymbol (BoxTypeSymbol (SampleEntry handlertype format)))
  => IsBox (SampleEntry handlertype format) where
    type BoxContent (SampleEntry handlertype format) =
      SampleEntryFields (SampleEntry handlertype format)
