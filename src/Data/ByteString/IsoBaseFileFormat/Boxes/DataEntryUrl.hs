-- | A data references URL.
module Data.ByteString.IsoBaseFileFormat.Boxes.DataEntryUrl
  (DataEntryUrl()
  ,localMediaDataEntryUrl
  ,dataEntryUrl
  )
  where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box
import Data.ByteString.IsoBaseFileFormat.Boxes.FullBox
import qualified Data.Text as T
import Data.Default

-- | A container for a URL or an indicator for local only media
newtype DataEntryUrl =
  DataEntryUrl (Maybe T.Text)
  deriving (Default,IsBoxContent)

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

instance IsBox DataEntryUrl
type instance BoxTypeSymbol DataEntryUrl = "url "
