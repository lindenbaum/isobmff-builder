-- | A single-track AAC audio streaming utility wrapping 'AudioFile' for
-- streaming via e.g. DASH.
module Data.ByteString.Mp4.AudioStreaming
  ( StreamingContext
  , getStreamConfig, getStreamBaseTime, getStreamSequence
  , addToBaseTime, getSegmentDuration
  , streamInit, streamInitUtc, streamNextSample, streamFlush, module X)

where

import Data.Time.Clock
import Data.ByteString.Mp4.AudioFile as X
import qualified Data.ByteString as BS

-- | Initiate the 'StreamingContext' and create the /MP4 init segment/.
-- This lives in 'IO' because it read the current time from the real world.
streamInit
  :: String
  -> Word32
  -> Bool
  -> SamplingFreqTable
  -> ChannelConfigTable
  -> IO (Builder, StreamingContext)
streamInit !trackTitle !segmentDurationMillis !sbr !rate !channels = do
  t <- mp4CurrentTime
  let !cfg = AacMp4StreamConfig t trackTitle sbr rate channels
      !dur = sampleRateToNumber rate * segmentDurationMillis `div` 1000
  return (buildAacMp4StreamInit cfg, StreamingContext cfg 0 0 dur [])

-- | Initiate the 'StreamingContext' and create the /MP4 init segment/.
-- This lives in 'IO' because it read the current time from the real world.
streamInitUtc -- TODO remove 'streamInit' in favor of this ??
  :: String
  -> UTCTime
  -> Word32
  -> Bool
  -> SamplingFreqTable
  -> ChannelConfigTable
  -> (Builder, StreamingContext)
streamInitUtc !trackTitle !availabilityStartTime !segmentDurationMillis !sbr !rate !channels =
  let !cfg = AacMp4StreamConfig t0 trackTitle sbr rate channels
      !t0  = utcToMp4 availabilityStartTime
      !dur = sampleRateToNumber rate * segmentDurationMillis `div` 1000
  in (buildAacMp4StreamInit cfg, StreamingContext cfg 0 0 dur [])

-- | Return the 'AacMp4StreamConfig' from an 'StreamingContext'
getStreamConfig :: StreamingContext -> AacMp4StreamConfig
getStreamConfig StreamingContext{..} = acConfig

-- | Return the current base decoding time
getStreamBaseTime :: StreamingContext -> Word32
getStreamBaseTime StreamingContext{..} = acBaseTime

-- | Return the current sequence number
getStreamSequence :: StreamingContext -> Word32
getStreamSequence StreamingContext{..} = acSequence

-- | Enqueue a sample, if enough samples are accumulated, generate the next segment
streamNextSample
  :: Word32
  -> BS.ByteString
  -> StreamingContext
  -> (Maybe Builder, StreamingContext)
streamNextSample !sampleCount !sample !ctx@StreamingContext{..} =
  let !segments = (sampleCount, sample) : acSegments
      !currentDuration = sum $ fst <$> segments
  in
    if currentDuration >= acSegmentDuration then
      let !tf = buildAacMp4TrackFragment $
                AacMp4TrackFragment acSequence acBaseTime (reverse segments)
          !ctx' = ctx { acSequence = acSequence + 1
                       , acBaseTime = acBaseTime + currentDuration
                       , acSegments = []}
      in (Just tf, ctx')
    else
      (Nothing, ctx{ acSegments = segments })

-- | Enqueue a sample, if enough samples are accumulated, generate the next segment
streamFlush
  ::  StreamingContext
  -> (Maybe Builder, StreamingContext)
streamFlush !ctx@StreamingContext{..} =
  let !currentDuration = sum $ fst <$> acSegments
  in
    if currentDuration > 0 then
      let !tf = buildAacMp4TrackFragment $
                AacMp4TrackFragment acSequence acBaseTime (reverse acSegments)
          !ctx' = ctx { acSequence = acSequence + 1
                       , acBaseTime = acBaseTime + currentDuration
                       , acSegments = []}
      in (Just tf, ctx')
    else (Nothing, ctx)

-- | Contains a sample in the ISO14496 style interpretation, i.e.
-- a smallish buffer of e.g. 20ms audio data or a single video frame.
-- A sample has some kind of time or at least order associated to it.
-- TODO not right now, add it
--
-- Also a sample has a duration measured as the sampleCount. TODO make this a
-- real data type, and possible refactor this to be a seperate issue from
-- filling stream gaps and determining the offsets and the decoding time stamp.

-- | Contains the configuration and state for the creation of a DASH audio
-- stream.
data StreamingContext =
  StreamingContext { acConfig          :: !AacMp4StreamConfig
                   , acSequence        :: !Word32
                   , acBaseTime        :: !Word32
                   , acSegmentDuration :: !Word32
                   , acSegments        :: ![(Word32, BS.ByteString)]
                   }

addToBaseTime :: StreamingContext -> NominalDiffTime -> StreamingContext
addToBaseTime !sc !dt =
  sc { acSequence = acSequence sc + deltaSegments
     , acBaseTime = acBaseTime sc + deltaBaseTime }
  where
    !deltaSegments = round (dt / segmentDuration)
    !deltaBaseTime = diffTimeToTicks dt timeScale
    !segmentDuration = ticksToDiffTime (acSegmentDuration sc) timeScale
    !timeScale = getAacMp4StreamConfigTimeScale (acConfig sc)

getSegmentDuration :: StreamingContext -> NominalDiffTime
getSegmentDuration !sc = segmentDuration
  where
    !segmentDuration = ticksToDiffTime (acSegmentDuration sc) timeScale
    !timeScale = getAacMp4StreamConfigTimeScale (acConfig sc)
