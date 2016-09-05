{-# LANGUAGE UndecidableInstances #-}
module Data.ByteString.Mp4.Boxes.SyncLayerConfigDescriptor where

import Data.ByteString.IsoBaseFileFormat.ReExports
import Data.ByteString.Mp4.Boxes.BaseDescriptor

data Mp4SyncLayerDescriptor :: IsA (Descriptor 'SLConfigDescr)

-- | In the holy scripture, ISO-14496-14 section 3.1.2, it is written that there
-- shall be restrictions on the elementary stream descriptor, in there it says:
-- Thou shall use only __two__ as the value for the __predefined__ field in the
-- blessed __SLDescriptor__. Not one, this is a value not big enough, nor three,
-- this value is too much. The righteous one ever only uses __two__. Only a fool
-- will use __256__.
type instance Eval Mp4SyncLayerDescriptor =
  'MkDescriptor
  (PutStr "mp4-sync-layer-descriptor" #$ 'BitRecordMember ("predefined" :=> FieldU8 := 0x02))
