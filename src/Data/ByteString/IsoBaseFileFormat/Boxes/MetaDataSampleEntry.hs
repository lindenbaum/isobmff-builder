module Data.ByteString.IsoBaseFileFormat.Boxes.MetaDataSampleEntry where

import Data.ByteString.IsoBaseFileFormat.Box
import Data.ByteString.IsoBaseFileFormat.Util.BoxFields
import Data.ByteString.IsoBaseFileFormat.Util.FullBox
import Data.ByteString.IsoBaseFileFormat.Boxes.Handler
import qualified Data.Text as T
import qualified Data.ByteString as B
import Data.Tagged ()
import Data.Default ()
import Data.ByteString.IsoBaseFileFormat.ReExports

-- * Generat meta data sample entry

type instance GetHandlerType (MetaDataCoding c) = 'TimedMetaDataTrack
type instance BoxTypeSymbol (MetaDataCoding c) = c

-- | A coproduct of meta data codings (XML, Text, ...)
data family MetaDataCoding (c :: Symbol)

-- * XML Meta Data

-- | The xml meta data samples declaration
newtype instance MetaDataCoding "metx" =
  XMLMetaDataSampleEntry
     (  Tagged "content_encoding" T.Text
     :+ Tagged "namespace"        T.Text
     :+ Tagged "schema_location"  T.Text
     :+                           Maybe (Box BitRate))
    deriving (IsBoxContent, Default)

-- * Text Meta Data

-- | The plain text meta data samples declaration
newtype instance MetaDataCoding "mett" =
  TextMetaDataSampleEntry
     (  Tagged "content_encoding" T.Text
     :+ Tagged "mime_format"      T.Text
     :+                           Maybe (Box BitRate))
    deriving (IsBoxContent, Default)

-- * URI based meta data

-- | The URI meta data samples declaration
newtype instance MetaDataCoding "urim" =
  UriMetaDataSampleEntry
     (  Tagged "the_label" (Box Uri)
     :+ Tagged "init"      (Maybe (Box UriInit))
     :+                    Maybe (Box BitRate))
    deriving (IsBoxContent, Default)

-- * Uri Box

-- | Contents of a 'Uri' box.
type Uri = FullBox UriField 0

-- | The URI that's inside the box
newtype UriField =
  UriField (Tagged "theURI" T.Text)
  deriving (Default, IsBoxContent)

-- | Make box with a UTF-8 URI.
uriBox :: T.Text -> Box Uri
uriBox t = fullBox 0 (UriField (Tagged t))

type instance BoxTypeSymbol UriField = "uri "

instance IsBox UriField

-- * Uri-Init Box

-- | Opaque data for the applications processing 'Uri' meta data.
newtype UriInitField =
  UriInitField (Tagged "uri_initialization_data" B.ByteString)
  deriving (Default, IsBoxContent)

-- | A 'FullBox' an UriInitField.
type UriInit = FullBox UriInitField 0

-- | Make box an 'UriInitField'
uriInitBox :: B.ByteString -> Box UriInit
uriInitBox = fullBox 0 . UriInitField . Tagged

type instance BoxTypeSymbol UriInitField = "uriI"

instance IsBox UriInitField

-- * Bit Rate Box

-- | Create a box for bit rates. Every 'SampleEntry' instance for meta data
-- may contain this box at the end.
bitRateBox :: BitRate -> Box BitRate
bitRateBox = Box

-- | Bitrate info for meta data samples
newtype BitRate =
  BitRate (U32 "bufferSizeDB" :+ U32 "maxBitrate" :+ U32 "avgBitrate")
  deriving (Default, IsBoxContent)

type instance BoxTypeSymbol BitRate = "btrt"

instance IsBox BitRate
