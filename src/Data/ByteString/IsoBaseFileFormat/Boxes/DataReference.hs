-- | A table of data references (URL/URNs). This tables is referred to by the
-- 'SampleDescription' this supports splitting a file over several files. When a
-- media file is split for transportation, this still counts as being in the
-- same file as the.
module Data.ByteString.IsoBaseFileFormat.Boxes.DataReference
  (DataReference()
  ,dataReference
  ,localMediaDataReference
  )
  where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box
import Data.ByteString.IsoBaseFileFormat.Boxes.FullBox
import Data.ByteString.IsoBaseFileFormat.Boxes.DataEntryUrl
import Data.ByteString.IsoBaseFileFormat.Boxes.BoxFields
       hiding (Default)
import Data.Singletons.Prelude.List (Length)
import Data.Default

-- | A container for 'DataEntryUrl's and 'DataEntryUrn's
newtype DataReference =
  DataReference (U32 "entry_count")
  deriving (Default,IsBoxContent)

-- | Create a 'DataReference' box.
dataReference
  :: KnownNat (Length ts)
  => Boxes ts -> Box (ContainerBox (FullBox DataReference 0) ts)
dataReference bs =
  containerBox (FullBox 0 $ DataReference (typeListLength bs))
               bs

-- | Create a 'DataReference' box with a single local media entry.
localMediaDataReference
  :: Box (ContainerBox (FullBox DataReference 0) '[Box (FullBox DataEntryUrl 0)])
localMediaDataReference = dataReference (singletonBox localMediaDataEntryUrl)

instance IsBox DataReference
type instance BoxTypeSymbol DataReference = "dref"
