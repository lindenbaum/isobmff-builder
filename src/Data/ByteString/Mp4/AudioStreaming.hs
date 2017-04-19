-- | A single-track AAC audio streaming utility wrapping 'AudioFile' for
-- streaming via e.g. DASH.
module Data.ByteString.Mp4.AudioStreaming
  (   Segment(..)
    , StreamingContext
    , AacMp4TrackFragment(..)
    , numberOfChannels
    , buildAacMp4TrackFragment
    , getStreamConfig, getStreamBaseTime, getStreamSequence
    , addToBaseTime, getSegmentDuration
    , streamInitINTERNAL_TESTING, streamInitUtc, streamNextSample, streamFlush
    , module X)

where

import Data.Time.Clock
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.ByteString.IsoBaseFileFormat.Box
import           Data.ByteString.IsoBaseFileFormat.Boxes
import           Data.ByteString.IsoBaseFileFormat.Brands.Dash    as X
import           Data.ByteString.IsoBaseFileFormat.MediaFile      as X
import           Data.ByteString.IsoBaseFileFormat.ReExports      as X
import           Data.ByteString.IsoBaseFileFormat.Util.BoxFields as X
import           Data.ByteString.IsoBaseFileFormat.Util.Time      as X
import           Data.ByteString.Mp4.Boxes.AudioSpecificConfig    as X
import           Data.ByteString.Mp4.Boxes.Mp4AudioSampleEntry    as X
import           Data.ByteString.Mp4.AacInitSegment               as X

-- | Contains the configuration and state for the creation of a DASH audio
-- stream.
data StreamingContext =
  StreamingContext { acConfig          :: !AacInitSegment
                   , acSegmentDuration :: !Word64 -- (in ticks)
                   , acSequence        :: !Word32
                   , acBaseTime        :: !Word64
                   , acSegments        :: ![(Word32, BS.ByteString)]
                   }

-- | Contains a sample in the ISO14496 style interpretation, i.e.
-- a smallish buffer of e.g. 20ms audio data or a single video frame.
-- A sample has some kind of time or at least order associated to it.
-- TODO not right now, add it
--
-- Also a sample has a duration measured as the sampleCount. TODO make this a
-- real data type, and possible refactor this to be a seperate issue from
-- filling stream gaps and determining the offsets and the decoding time stamp.
data Segment =
  Segment
  { segmentSequence :: !Word32
  , segmentTime     :: !Word64
  , segmentData     :: !BS.ByteString
  }

-- | Media fragment segment parameters of an aac audio stream mp4 file.
data AacMp4TrackFragment =
  AacMp4TrackFragment { fragmentSampleSequence      :: !Word32
                      , fragmentBaseMediaDecodeTime :: !Word64
                      , fragmentSamples             :: ![(Word32, BS.ByteString)]
                      }

numberOfChannels :: Num a => StreamingContext -> a
numberOfChannels = channelConfigToNumber . channelConfig . acConfig

addToBaseTime :: StreamingContext -> NominalDiffTime -> StreamingContext
addToBaseTime !sc !dt =
  sc { acSequence = newSequence
     , acBaseTime = diffTimeToTicks newBaseTime timeScale}
  where
    !timeScale = getAacMp4StreamConfigTimeScale (acConfig sc)
    !newSequence = round (newBaseTime / segmentDuration)
      where
        !segmentDuration = ticksToDiffTime (acSegmentDuration sc) timeScale
    !newBaseTime = dt + baseTime
      where
        !baseTime = ticksToDiffTime (acBaseTime sc) timeScale


-- | Return the 'AacInitSegment' from an 'StreamingContext'
getStreamConfig :: StreamingContext -> AacInitSegment
getStreamConfig StreamingContext{..} = acConfig

-- | Return the current base decoding time
getStreamBaseTime :: StreamingContext -> Word64
getStreamBaseTime StreamingContext{..} = acBaseTime

-- | Return the current sequence number
getStreamSequence :: StreamingContext -> Word32
getStreamSequence StreamingContext{..} = acSequence

instance Show Segment where
  show (Segment !s !t !d) =
    printf "SEGMENT - seq: %14d - time: %14d - size: %14d" s t (BS.length d)


getSegmentDuration :: StreamingContext -> NominalDiffTime
getSegmentDuration !sc = segmentDuration
  where
    !segmentDuration = ticksToDiffTime (acSegmentDuration sc) timeScale
    !timeScale = getAacMp4StreamConfigTimeScale (acConfig sc)

sampleCountDuration :: AacInitSegment -> Word32 -> Word64
sampleCountDuration (AacInitSegment _ _ _ _ _c) r =
  fromIntegral r -- `div` channelConfigToNumber c TODO clean this whole mess up and remove all Word32/Word64

getAacMp4StreamConfigTimeScale :: AacInitSegment -> TimeScale
getAacMp4StreamConfigTimeScale AacInitSegment{..} =
  TimeScale (sampleRateToNumber sampleRate)

-- | Initiate the 'StreamingContext' and create the /MP4 init segment/.
-- This lives in 'IO' because it read the current time from the real world.
streamInitINTERNAL_TESTING
  :: String
  -> NominalDiffTime
  -> Bool
  -> SamplingFreqTable
  -> ChannelConfigTable
  -> IO (BinaryAacInitSegment, StreamingContext)
streamInitINTERNAL_TESTING !trackTitle !segmentDuration !sbr !rate !channels = do
  t <- mp4CurrentTime
  let !cfg = AacInitSegment t trackTitle sbr rate channels
      !dur = diffTimeToTicks segmentDuration timeScale
      !timeScale = getAacMp4StreamConfigTimeScale cfg
  return ( buildAacInitSegment cfg
         , StreamingContext cfg dur 0 0 [])

-- | Initiate the 'StreamingContext' and create the /MP4 init segment/.
-- This lives in 'IO' because it read the current time from the real world.
streamInitUtc -- TODO remove 'streamInit' in favor of this ??
  :: String
  -> UTCTime
  -> UTCTime
  -> NominalDiffTime
  -> Bool
  -> SamplingFreqTable
  -> ChannelConfigTable
  -> (BinaryAacInitSegment, StreamingContext)
streamInitUtc !trackTitle !availabilityStartTime !refTime !segmentDuration !sbr !rate !channels =
  let !cfg = AacInitSegment t0 trackTitle sbr rate channels
      !t0  = utcToMp4 availabilityStartTime
      !secondsSinceRef = diffUTCTime availabilityStartTime refTime
      !dur = diffTimeToTicks segmentDuration timeScale
      !timeScale = getAacMp4StreamConfigTimeScale cfg
  in ( buildAacInitSegment cfg
     , addToBaseTime (StreamingContext cfg dur 0 0 []) secondsSinceRef)

-- | Enqueue a sample, if enough samples are accumulated, generate the next segment
streamNextSample
  :: Word32
  -> BS.ByteString
  -> StreamingContext
  -> (Maybe Segment, StreamingContext)
streamNextSample !sampleCount !sample !ctx@StreamingContext{..} =
  let !segments       = (sampleCount, sample) : acSegments
      !currentEndTime = sampleCountDuration acConfig (sum (fst <$> segments)) + acBaseTime
      !nextBaseTime   = (fromIntegral acSequence + 1) * acSegmentDuration64
      !acSegmentDuration64 = fromIntegral acSegmentDuration
  in
    if currentEndTime >= nextBaseTime then
      let !ctx' =
            ctx { acSequence = fromIntegral (currentEndTime `div` acSegmentDuration64)
                , acBaseTime = currentEndTime
                , acSegments = []
                }
          -- Output to write into fragment:
          !tf = buildAacMp4TrackFragment $
                AacMp4TrackFragment acSequence acBaseTime (reverse segments)

      in (Just (Segment acSequence acBaseTime (BL.toStrict (toLazyByteString tf))), ctx')
    else
      (Nothing, ctx{ acSegments = segments })

-- | Enqueue a sample, if enough samples are accumulated, generate the next segment
streamFlush
  ::  StreamingContext
  -> (Maybe Segment, StreamingContext)
streamFlush !ctx@StreamingContext{..} =
  let currentEndTime =
        sampleCountDuration acConfig (sum (fst <$> acSegments)) + acBaseTime
  in
    if not (null acSegments) then
      let !tf = buildAacMp4TrackFragment $
                AacMp4TrackFragment acSequence acBaseTime (reverse acSegments)
          !ctx' = ctx { acSequence = fromIntegral
                        (currentEndTime `div` fromIntegral acSegmentDuration)
                      , acBaseTime = currentEndTime
                      , acSegments = []}
      in (Just (Segment acSequence acBaseTime (BL.toStrict (toLazyByteString tf))), ctx')
    else (Nothing, ctx)

-- | Convert a 'AacMp4TrackFragment record to a generic 'Boxes' collection.
buildAacMp4TrackFragment
  :: AacMp4TrackFragment -> Builder
buildAacMp4TrackFragment AacMp4TrackFragment{..} =
    mediaBuilder dash
      (  styp
      :. movieFragment
           (  mfhd
           :| trackFragment
                (  tfhd
                :. tfdt
                :| trun))
      :| mdat)
  where
    !styp = segmentTypeBox               (SegmentType "msdh" 0 ["msdh","dash"])
    !mfhd = movieFragmentHeader          (MovieFragmentHeader (Scalar fragmentSampleSequence))
    !tfhd = trackFragmentHeader          def
    !tfdt = trackFragBaseMediaDecodeTime (TSv1 fragmentBaseMediaDecodeTime)
    !trun = trackRunIso5                 mdatOffset fragmentSamples
      where
        !mdatOffset = movieFragmentStaticSize
                     + movieFragmentHeaderStaticSize
                     + trackFragmentStaticSize
                     + trackFragmentHeaderStaticSize
                     + trackFragBaseMediaDecodeTimeStaticSize64
    !mdat = mediaData (MediaData (BS.concat (snd <$> fragmentSamples)))
