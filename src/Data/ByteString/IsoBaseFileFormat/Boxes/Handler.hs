{-# LANGUAGE UndecidableInstances #-}
-- | A 'Handler' box describes how a track is presentation and by which sensory
-- process it is perceived, e.g. audio, video, text, see 'HandlerType'
module Data.ByteString.IsoBaseFileFormat.Boxes.Handler
  (Handler()
  ,handler
  ,audioTrackHandler
  ,namedAudioTrackHandler
  ,videoTrackHandler
  ,namedVideoTrackHandler
  ,hintTrackHandler
  ,namedHintTrackHandler
  ,timedMetadataTrackHandler
  ,namedTimedMetadataTrackHandler
  ,auxilliaryVideoTrackHandler
  ,namedAuxilliaryVideoTrackHandler
  ,HandlerType(..)
  ,HandlerTypeCode)
  where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box
import Data.ByteString.IsoBaseFileFormat.Boxes.BoxFields
import Data.ByteString.IsoBaseFileFormat.Boxes.FullBox
import qualified Data.Text as T

-- | Handler box fields. A handler box may also contain a null terminated
-- description text in UTF-8. The 'T.Text' parameter is a human readable name of
-- the track type for debugging.
newtype Handler (t :: HandlerType) where
        Handler ::
          Constant (U32 "pre_defined") 0 :+
            Constant (U32Text "t") (HandlerTypeCode t) :+
              Constant (U32Arr "reserved" 3) '[0, 0, 0] :+
                Template T.Text "" -> Handler t

deriving instance (KnownSymbol (HandlerTypeCode t)) => IsBoxContent (Handler t)
deriving instance Default (Handler t)

-- | A 'Handler' can be exactly one of these.
data HandlerType
  = VideoTrack
  | AudioTrack
  | HintTrack
  | TimedMetadataTrack
  | AuxilliaryVideoTrack

type family HandlerTypeCode (handlertype :: HandlerType) :: Symbol where
  HandlerTypeCode 'VideoTrack           = "vide"
  HandlerTypeCode 'AudioTrack           = "soun"
  HandlerTypeCode 'HintTrack            = "hint"
  HandlerTypeCode 'TimedMetadataTrack   = "meta"
  HandlerTypeCode 'AuxilliaryVideoTrack = "auxv"

-- | Create a 'Handler' box.
handler
  :: (KnownSymbol (HandlerTypeCode t))
  => Handler t -> Box (FullBox (Handler t) 0)
handler = fullBox 0

-- | Create 'Handler' fields for 'AudioTrack's with @"Audio Track"@ as @name@.
audioTrackHandler :: Handler 'AudioTrack
audioTrackHandler = namedAudioTrackHandler "Audio Track"

-- | Create 'Handler' fields for 'AudioTrack's, with the given name.
namedAudioTrackHandler :: T.Text -> Handler 'AudioTrack
namedAudioTrackHandler txt = Handler (def :+ def :+ def :+ Custom txt)

-- | Create 'Handler' fields for 'VideoTrack's with @"Video Track"@ as @name@.
videoTrackHandler :: Handler 'VideoTrack
videoTrackHandler = namedVideoTrackHandler "Video Track"

-- | Create 'Handler' fields for 'VideoTrack's, with the given name.
namedVideoTrackHandler :: T.Text -> Handler 'VideoTrack
namedVideoTrackHandler txt = Handler (def :+ def :+ def :+ Custom txt)

-- | Create 'Handler' fields for 'HintTrack's with @"Hint Track"@ as @name@.
hintTrackHandler :: Handler 'HintTrack
hintTrackHandler = namedHintTrackHandler "Hint Track"

-- | Create 'Handler' fields for 'HintTrack's, with the given name.
namedHintTrackHandler :: T.Text -> Handler 'HintTrack
namedHintTrackHandler txt = Handler (def :+ def :+ def :+ Custom txt)

-- | Create 'Handler' fields for 'TimedMetadataTrack's with @"TimedMetadata
-- Track"@ as @name@.
timedMetadataTrackHandler :: Handler 'TimedMetadataTrack
timedMetadataTrackHandler =
  namedTimedMetadataTrackHandler "Timed Metadata Track"

-- | Create 'Handler' fields for 'TimedMetadataTrack's, with the given name.
namedTimedMetadataTrackHandler :: T.Text -> Handler 'TimedMetadataTrack
namedTimedMetadataTrackHandler txt =
  Handler (def :+ def :+ def :+ Custom txt)

-- | Create 'Handler' fields for 'AuxilliaryVideoTrack's with
-- @"AuxilliaryVideoTrack Track"@ as @name@.
auxilliaryVideoTrackHandler :: Handler 'AuxilliaryVideoTrack
auxilliaryVideoTrackHandler =
  namedAuxilliaryVideoTrackHandler "Timed Metadata Track"

-- | Create 'Handler' fields for 'AuxilliaryVideoTrack's, with the given name.
namedAuxilliaryVideoTrackHandler :: T.Text -> Handler 'AuxilliaryVideoTrack
namedAuxilliaryVideoTrackHandler txt =
  Handler (def :+ def :+ def :+ (Custom txt))

instance (KnownSymbol (HandlerTypeCode t)) => IsBox (Handler t)

type instance BoxTypeSymbol (Handler t) = "hdlr"
