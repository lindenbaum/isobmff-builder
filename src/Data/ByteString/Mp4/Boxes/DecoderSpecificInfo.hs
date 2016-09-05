{-# LANGUAGE UndecidableInstances #-}
module Data.ByteString.Mp4.Boxes.DecoderSpecificInfo where

import           Data.ByteString.Mp4.Boxes.BaseDescriptor
import           Data.Type.BitRecords
import           Data.Type.Pretty
import           Data.Kind.Extra

-- * Abstract class for opaque object- and stream type dependent decoder
-- settings.
data DecoderSpecificInfo where
  MkDecoderSpecificInfo :: BitRecord -> DecoderSpecificInfo

type instance Eval ('MkDecoderSpecificInfo body ~~> Descriptor 'DecSpecificInfo) =
  'MkDescriptor (PutStr "decoder-specific-info" #$ body)

type instance ToBitRecord  (x :: IsA DecoderSpecificInfo) =
  ToBitRecord (x --> Descriptor 'DecSpecificInfo)
