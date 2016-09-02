{-# LANGUAGE UndecidableInstances #-}
module Data.ByteString.Mp4.Boxes.DecoderSpecificInfo where

import           Data.ByteString.Mp4.Boxes.BaseDescriptor
import           Data.Type.BitRecords
import           Data.Type.Pretty

-- * Abstract class for opaque object- and stream type dependent
-- decoder settings.
type family
  DecoderSpecificInfo (body :: BitRecord) :: BitRecord where
  DecoderSpecificInfo body =
    BaseDescriptor
      'DecSpecificInfo
      ('ReplacePretty
        ("decoder-specific-info" <:$$--> PrettyRecord body)
        body)
