{-# LANGUAGE UndecidableInstances #-}
module Data.ByteString.Mp4.Boxes.DecoderSpecificInfo where

import           Data.ByteString.Mp4.Boxes.BaseDescriptor
import           Data.Type.BitRecords
import           Data.Type.Pretty


-- * Abstract class for opaque object- and stream type dependent decoder
-- settings.
data DecoderSpecificInfoImpl

type family MkDecoderSpecificInfoImpl (di :: DecoderSpecificInfoImpl) :: body

data DecoderSpecificInfo :: BitRecord -> BaseDescriptorImpl 'DecSpecificInfo
type instance MkBaseDescriptor



  MkBaseDescriptor 'DecSpecificInfo (body -># PutStr "decoder-specific-info")

type family
