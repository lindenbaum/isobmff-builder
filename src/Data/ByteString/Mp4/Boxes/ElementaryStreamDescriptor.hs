{-# LANGUAGE UndecidableInstances #-}
module Data.ByteString.Mp4.Boxes.ElementaryStreamDescriptor where

import           Data.ByteString.IsoBaseFileFormat.ReExports
import           Data.Type.BitRecords
import           Data.ByteString.Mp4.Boxes.BaseDescriptor



type ESDescriptorAudioContent dependsOnEsId =
                     "esId" :=> Word16
  :>: (FlagJust dependsOnEsId)
  :>: (FlagJust url)
  :>: (FlagJust ocrEsId)
  :>: "streamPriority"       :=> Field 5
  :>: dependsOnEsId
  :>: url
  :>: ocrEsId


-- * Interface from ISO 14496-3 (Audio)

staticESDescriptorAudio :: StaticBaseDescriptorWithArgs (ES_DescrP ESDescriptorAudioContent)
staticESDescriptorAudio =
    staticBaseDescriptorWithArgs (ES_DescrP :: ES_DescrP ESDescriptorAudioContent)

type StaticESDescriptorAudio = StaticBaseDescriptor (ES_DescrP ESDescriptorAudioContent)

type AudioSpecificConfig audiConfig = ()

type AudioObjectType n = If (n <=? 30) (AudioObjectTypeSmall n) (AudioObjectTypeExt n)

type AudioObjectTypeSmall n = "audioObjectType" :=> Field 5 := n

type AudioObjectTypeExt n = AudioObjectTypeSmall 31 :>: "audioObjectTypeExt" :=> Field 6 := (n - 32)
