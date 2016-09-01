{-# LANGUAGE UndecidableInstances #-}
module Data.ByteString.Mp4.Boxes.DecoderSpecificInfo where


import           Data.ByteString.Mp4.Boxes.BaseDescriptor
import           Data.Type.BitRecords
import           Data.Type.Pretty

-- * Abstract class for 'DecoderSpecificInfo'
type family
  DecoderSpecificInfo (body :: BitRecord) :: BitRecord where
  DecoderSpecificInfo body =
    BaseDescriptor
       'DecSpecificInfo
       ('ReplacePretty
         ("DecoderSpecificInfo" <:$$--> PrettyRecord body)
         body)
