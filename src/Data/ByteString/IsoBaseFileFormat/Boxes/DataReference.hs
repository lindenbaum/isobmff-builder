-- | A table of data references (URL/URNs). This tables is referred to by the
-- 'SampleDescription' this supports splitting a file over several files. When a
-- media file is split for transportation, this still counts as being in the
-- same file as the.
module Data.ByteString.IsoBaseFileFormat.Boxes.DataReference
  (DataReference()
  ,DataEntryUrl()
  ,DataEntryUrn()
  ,dataReference
  ,localMediaDataReference
  ,localMediaDataEntryUrl
  ,dataEntryUrl
  ,dataEntryUrn
  )
  where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box
import Data.ByteString.IsoBaseFileFormat.Boxes.FullBox
import Data.ByteString.IsoBaseFileFormat.Boxes.BoxFields
       hiding (Default)
import qualified Data.Text as T
import Data.Singletons.Prelude.List (Length)
import Data.Default

-- | A container for 'DataEntryUrl's and 'DataEntryUrn's
newtype DataReference =
  DataReference (U32 "entry_count")
  deriving (Default,IsBoxContent)

-- | A container for a URL
newtype DataEntryUrl =
  DataEntryUrl (Maybe T.Text)
  deriving (Default,IsBoxContent)

-- | A container for a URN and optionally a URL
newtype DataEntryUrn =
  DataEntryUrn (T.Text :+ T.Text)
  deriving (IsBoxContent)

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

-- | Create a 'DataEntryUrl' box for local media entry with the flag set and
-- empty content.
localMediaDataEntryUrl
  :: Box (FullBox DataEntryUrl 0)
localMediaDataEntryUrl = dataEntryUrl True Nothing

-- | Create a 'DataEntryUrl' box. The flag determines if the url is local, i.e.
-- the media data is in the same file.
dataEntryUrl
  :: Bool -> Maybe T.Text -> Box (FullBox DataEntryUrl 0)
dataEntryUrl isLocal Nothing = Box (FullBox (fromIntegral $ fromEnum isLocal) $ DataEntryUrl Nothing)
dataEntryUrl isLocal murl = Box (FullBox (fromIntegral $ fromEnum isLocal) $ DataEntryUrl murl)

-- | Create a 'DataEntryUrn' box. The flag determines if the url is local, i.e.
-- the media data is in the same file.
dataEntryUrn
  :: Bool -> T.Text -> T.Text -> Box (FullBox DataEntryUrn 0)
dataEntryUrn isLocal urn url = Box (FullBox (fromIntegral $ fromEnum isLocal) $ DataEntryUrn $ urn :+ url)

instance IsBox DataReference
type instance BoxTypeSymbol DataReference = "dref"

instance IsBox DataEntryUrn
type instance BoxTypeSymbol DataEntryUrn = "urn "

instance IsBox DataEntryUrl
type instance BoxTypeSymbol DataEntryUrl = "urn "
