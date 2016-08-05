{-# LANGUAGE UndecidableInstances #-}
-- | Connect a sample description to a data reference via an index into the data
-- reference entry table.
module Data.ByteString.IsoBaseFileFormat.Boxes.SampleEntry where

import Data.ByteString.IsoBaseFileFormat.Box
import Data.ByteString.IsoBaseFileFormat.Boxes.Handler
import Data.ByteString.IsoBaseFileFormat.Util.BoxFields
import Data.ByteString.IsoBaseFileFormat.ReExports

-- | Create a 'SampleEntry' 'Box' from the data reference index and the
-- 'HandlerType' specific 'SampleEntry' instance.
sampleEntry
  :: U16 "data_reference_index"
  -> handlerSpecific
  -> Box (SampleEntry handlerSpecific)
sampleEntry i se = Box (SampleEntry (Constant :+ i :+ se))

-- | A common header for all specific sample entries, the 'BoxContent' of the
-- abstract 'SampleEntry' is @SampleEntry (SampleEntry h f)@.
newtype SampleEntry handlerSpecific where
  SampleEntry
    :: Constant (U8Arr "reserved" 6) '[0,0,0,0,0,0]
    :+ U16 "data_reference_index"
    :+ handlerSpecific
    -> SampleEntry handlerSpecific

deriving instance IsBoxContent handlerSpecific =>
  IsBoxContent (SampleEntry handlerSpecific)

deriving instance Default handlerSpecific =>
  Default (SampleEntry handlerSpecific)

-- | Use this in 'IsMediaFileFormat's 'BoxLayout' to range over any specific
-- 'SampleEntry', disregarding the second parameter (that indicates low-level
-- format, protocol or codec characteristics). Add a 'GetHandlerType' instance for
-- 'MatchSampleEntry' to match a sample type specific entry.
data MatchSampleEntry :: HandlerType -> Type

type instance
  IsRuleConform (Box (SampleEntry handlerSpecificEntry))
                (MatchSampleEntry handlerType) =
    HandlerTypeCode (GetHandlerType handlerSpecificEntry)
      == HandlerTypeCode handlerType

-- | The 'BoxTypeSymbol' of sample entry is exactly the @format@ type index of
            -- the 'SampleEntry' family.
type instance
  BoxTypeSymbol (SampleEntry handlerSpecific) = BoxTypeSymbol handlerSpecific

instance (IsBoxContent handlertype
         ,KnownSymbol (BoxTypeSymbol handlertype))
  => IsBox (SampleEntry handlertype)
