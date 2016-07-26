-- | A table of data references (URL/URNs). This tables is referred to by the
-- 'SampleDescription' this supports splitting a file over several files.
module Data.ByteString.IsoBaseFileFormat.Boxes.DataReference (DataReference(..),
  DataEntry(..), dataReference, localMediaDataReference) where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box
import Data.ByteString.IsoBaseFileFormat.Boxes.BoxFields
import Data.ByteString.IsoBaseFileFormat.Boxes.FullBox
import qualified Data.Text as T
import Data.List (foldl')

-- | A container for 'DataEntry's
data DataReference

-- | Create a 'DataReference' box.
dataReference
  :: Box (FullBox 0 DataReference)
dataReference = fullBox 0 ()

-- | Create a 'DataReference' box with a single local media entry.
localMediaDataReference
  :: Box (FullBox 0 DataReference)
localMediaDataReference = undefined

instance IsBoxType DataReference where
  toBoxType _ _ = StdType "dref"

-- | Either a URN, a URL or an empty box. The empty box indicates that the media
-- is located in the same file as this box. NOTE: When a media file is split
-- for transportation, this still counts as being in the same file as the.
data DataEntry
  = LocalMediaEntry
  | UrlEntry {dataEntryUrl  :: T.Text}
  | UrnEntry {dataEntryName :: T.Text
             ,dataEntryUrl  :: T.Text}
