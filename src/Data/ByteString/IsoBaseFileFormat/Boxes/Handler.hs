-- | A 'Handler' box describes how a track is presentation and by which sensory
-- process it is perceived, e.g. audio, video, text, see 'HandlerType'
module Data.ByteString.IsoBaseFileFormat.Boxes.Handler where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box
import Data.ByteString.IsoBaseFileFormat.Boxes.BoxFields
import Data.ByteString.IsoBaseFileFormat.Boxes.FullBox
import Data.Default
import qualified Data.Text as T

-- | Handler box fields. A handler box may also contain a null terminated
-- description text in UTF-8.
newtype Handler where
  Handler :: Constant (U32 "pre_defined") 0
          :+ HandlerType
          :+ Constant (U32Arr "reserved" 3) '[0,0,0]
          :+ Template T.Text ""
          -> Handler
  deriving (IsBoxContent, Default)

-- | A 'Handler' can be exactly one of these.
data HandlerType =
    VideoTrack
  | AudioTrack
  | HintTrack
  | TimedMetadataTrack
  | AuxilliaryVideoTrack

-- | Create a 'Handler' box.
handler
  :: Handler -> Box (FullBox 0 Handler)
handler = fullBox 0

instance IsBoxType Handler where
  type BoxContent Handler = Handler
  toBoxType _ _ = StdType "hdlr"

instance Default HandlerType where
  def = AudioTrack

instance IsBoxContent HandlerType where
  boxSize    _                    = 4
  boxBuilder VideoTrack           = boxBuilder (FourCc ('v','i','d','e'))
  boxBuilder AudioTrack           = boxBuilder (FourCc ('s','o','u','n'))
  boxBuilder HintTrack            = boxBuilder (FourCc ('h','i','n','t'))
  boxBuilder TimedMetadataTrack   = boxBuilder (FourCc ('m','e','t','a'))
  boxBuilder AuxilliaryVideoTrack = boxBuilder (FourCc ('a','u','x','v'))
