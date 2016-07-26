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
data DataReference =
  DataReference DataEntry [DataEntry]

-- | Either a URN, a URL or an empty box. The empty box indicates that the media
-- is located in the same file as this box. NOTE: When a media file is split
-- for transportation, this still counts as being in the same file as the.
data DataEntry
  = LocalMediaEntry
  | UrlEntry {dataEntryUrl  :: T.Text}
  | UrnEntry {dataEntryName :: T.Text
             ,dataEntryUrl  :: T.Text}

-- | Create a 'DataReference' box.
dataReference
  :: ValidContainerBox brand DataReference '[]
  => DataReference -> Box brand DataReference
dataReference = closedFullBox Default 0

-- | Create a 'DataReference' box with a single local media entry.
localMediaDataReference
  :: ValidContainerBox brand DataReference '[]
  => Box brand DataReference
localMediaDataReference = dataReference (DataReference LocalMediaEntry [])

instance IsBoxType DataReference where
  type BoxContent DataReference = FullBox 0 DataReference
  toBoxType _ _ = StdType "dref"

instance IsBoxContent DataReference where
  boxSize (DataReference e es') = 4 + sum es
    where es = boxSize <$> e:es'
  boxBuilder (DataReference e es') = foldl' (<>) (word32BE (fromIntegral (length es))) es
    where es = boxBuilder <$> e:es'

instance IsBoxContent DataEntry where
  boxSize LocalMediaEntry =
    boxSize (FullBox (Custom 0) 1 urlCc :: FullDataEntryLocal)
  boxSize (UrlEntry url) =
    boxSize (FullBox (Custom 0) 0 (urlCc :+ url) :: FullDataEntryUrl)
  boxSize (UrnEntry name url) =
    boxSize (FullBox (Custom 0) 0 (urnCc :+ name :+ url) :: FullDataEntryUrn)
  boxBuilder LocalMediaEntry =
    boxBuilder (FullBox (Custom 0) 1 urlCc :: FullDataEntryLocal)
  boxBuilder (UrlEntry url) =
    boxBuilder (FullBox (Custom 0) 0 (urlCc :+ url) :: FullDataEntryUrl)
  boxBuilder (UrnEntry name url) =
    boxBuilder (FullBox (Custom 0) 0 (urnCc :+ name :+ url) :: FullDataEntryUrn)




-- data DataEntryUrl =
--   LocalMediaEntry
--   | DataEntryUrl T.Text
--
-- instance IsBoxType DataEntryUrl where
--   type BoxContent DataEntryUrl = FullBox 0 DataEntryUrl
--   toBoxType _ _ = StdType "url "
--


urlCc,urnCc :: FourCc
urlCc = "url "
urnCc = "urn "

type FullDataEntryLocal = FullBox 0 FourCc
type FullDataEntryUrl   = FullBox 0 (FourCc :+ T.Text)
type FullDataEntryUrn   = FullBox 0 (FourCc :+ T.Text :+ T.Text)
