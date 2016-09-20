-- | A single-track AAC audio streaming utility wrapping 'AudioFile' for
-- streaming via e.g. DASH.
module Data.ByteString.Mp4.AudioStreaming
  (AacStreamingContext, aacStreamingBegin, aacStreamNextSample, aacStreamFlush, module X)

where

import Data.ByteString.Mp4.AudioFile as X
import qualified Data.ByteString as BS

-- | Contains the configuration and state for the creation of a DASH audio
-- stream.
data AacStreamingContext =
  AacStreamingContext { acConfig          :: !AacMp4StreamConfig
                      , acSequence        :: !Word32
                      , acBaseTime        :: !Word32
                      , acSegmentDuration :: !Word32
                      , acSegments        :: ![(Word32, BS.ByteString)]
                      }

-- | Initiate the 'AacStreamingContext' and create the /MP4 init segment/.
-- This lives in 'IO' because it read the current time from the real world.
aacStreamingBegin
  :: String
  -> Word32
  -> Bool
  -> SamplingFreqTable
  -> ChannelConfigTable
  -> IO (Builder, AacStreamingContext)
aacStreamingBegin !trackTitle !segmentDurationMillis !sbr !rate !channels = do
  t <- mp4CurrentTime
  let !cfg = AacMp4StreamConfig t trackTitle sbr rate channels
      !dur = sampleRateToNumber rate * segmentDurationMillis `div` 1000
  return (buildAacMp4StreamInit cfg, AacStreamingContext cfg 0 0 dur [])

-- | Enqueue a sample, if enough samples are accumulated, generate the next segment
aacStreamNextSample
  :: Word32
  -> BS.ByteString
  -> AacStreamingContext
  -> (Maybe Builder, AacStreamingContext)
aacStreamNextSample !duration !sample !ctx@AacStreamingContext{..} =
  let !segments = (duration, sample) : acSegments
      !currentDuration = sum $ fst <$> segments
  in
    if currentDuration >= acSegmentDuration then
      let !tf = buildAacMp4TrackFragment $
                AacMp4TrackFragment acSequence acBaseTime (reverse segments)
          !ctx' = ctx { acSequence = acSequence + fromIntegral (length segments)
                       , acBaseTime = acBaseTime + currentDuration
                       , acSegments = []}
      in (Just tf, ctx')
    else
      (Nothing, ctx{ acSegments = segments })

-- | Enqueue a sample, if enough samples are accumulated, generate the next segment
aacStreamFlush
  ::  AacStreamingContext
  -> (Maybe Builder, AacStreamingContext)
aacStreamFlush !ctx@AacStreamingContext{..} =
  let !currentDuration = sum $ fst <$> acSegments
  in
    if currentDuration > 0 then
      let !tf = buildAacMp4TrackFragment $
                AacMp4TrackFragment acSequence acBaseTime (reverse acSegments)
          !ctx' = ctx { acSequence = acSequence + fromIntegral (length acSegments)
                       , acBaseTime = acBaseTime + currentDuration
                       , acSegments = []}
      in (Just tf, ctx')
    else (Nothing, ctx)
