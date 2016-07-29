-- | A table of data references (URN with optional URL.
module Data.ByteString.IsoBaseFileFormat.Boxes.DataEntryUrn
  (DataEntryUrn()
  ,dataEntryUrn
  )
  where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box
import Data.ByteString.IsoBaseFileFormat.Boxes.FullBox
import qualified Data.Text as T

-- | A container for a URN and optionally a URL
newtype DataEntryUrn =
  DataEntryUrn (T.Text :+ T.Text)
  deriving (IsBoxContent)

-- | Create a 'DataEntryUrn' box. The flag determines if the url is local, i.e.
-- the media data is in the same file.
dataEntryUrn
  :: Bool -> T.Text -> T.Text -> Box (FullBox DataEntryUrn 0)
dataEntryUrn isLocal urn url = Box (FullBox (fromIntegral $ fromEnum isLocal) $ DataEntryUrn $ urn :+ url)

instance IsBox DataEntryUrn
type instance BoxTypeSymbol DataEntryUrn = "urn "
