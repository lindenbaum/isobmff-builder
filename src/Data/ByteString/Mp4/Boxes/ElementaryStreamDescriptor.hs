{-# LANGUAGE UndecidableInstances #-}

module Data.ByteString.Mp4.Boxes.ElementaryStreamDescriptor where

import Data.ByteString.IsoBaseFileFormat.Box
import Data.ByteString.IsoBaseFileFormat.ReExports
import Data.Type.BitRecords
import Data.ByteString.Mp4.Boxes.BaseDescriptor
import Data.ByteString.Mp4.Boxes.Expandable

-- * Decoder Specific Info
-- * Audio Object type -- TODO move into modules
type AudioSpecificConfig audiConfig = ()

type AudioObjectType n =
  If (n <=? 30) (AudioObjectTypeSmall n) (AudioObjectTypeExt n)

type AudioObjectTypeSmall n = "audioObjectType" :=> Field 5 := n

type AudioObjectTypeExt n =
  AudioObjectTypeSmall 31 :>: "audioObjectTypeExt" :=> Field 6 := (n - 32)

type ElementaryStreamDescriptor =
  StaticExpandable MinimalAudioES_Descriptor -- TODO allow full-on yada yada yada EDDesc

type MinimalAudioES_Descriptor =
  "ES_ID" :=> Word16 :>: "streamDependenceFlag" :=> Flag :>: 'False :>: "OCRstreamFlag" :=> Bool :>: "streamPriority" :=> Field 5
