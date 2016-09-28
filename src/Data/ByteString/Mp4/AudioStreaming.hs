-- | A single-track AAC audio streaming utility wrapping 'AudioFile' for
-- streaming via e.g. DASH.
module Data.ByteString.Mp4.AudioStreaming
  (   Segment(..)
    , InitSegment(..)
    , StreamingContext, AacMp4StreamConfig(..)
    , AacMp4TrackFragment(..)
    , buildAacMp4TrackFragment
    , buildAacMp4StreamInit
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
import           Data.ByteString.IsoBaseFileFormat.Util.Versioned
import           Data.ByteString.Mp4.Boxes.AudioSpecificConfig    as X
import           Data.ByteString.Mp4.Boxes.Mp4AudioSampleEntry    as X
import qualified Data.Text                                        as T


-- | Initiate the 'StreamingContext' and create the /MP4 init segment/.
-- This lives in 'IO' because it read the current time from the real world.
streamInitINTERNAL_TESTING
  :: String
  -> NominalDiffTime
  -> Bool
  -> SamplingFreqTable
  -> ChannelConfigTable
  -> IO (InitSegment, StreamingContext)
streamInitINTERNAL_TESTING !trackTitle !segmentDuration !sbr !rate !channels = do
  t <- mp4CurrentTime
  let !cfg = AacMp4StreamConfig t trackTitle sbr rate channels
      !dur = diffTimeToTicks segmentDuration timeScale
      !timeScale = getAacMp4StreamConfigTimeScale cfg
  return (InitSegment
          (BL.toStrict (toLazyByteString (buildAacMp4StreamInit cfg)))
         , StreamingContext cfg dur 0 0 [])

newtype InitSegment =
  InitSegment
  { fromInitSegment :: BS.ByteString }

instance Show InitSegment where
  show (InitSegment !d) =
    printf "INIT SEGMENT - size: %14d" (BS.length d)

-- | Initiate the 'StreamingContext' and create the /MP4 init segment/.
-- This lives in 'IO' because it read the current time from the real world.
streamInitUtc -- TODO remove 'streamInit' in favor of this ??
  :: String
  -> UTCTime
  -> NominalDiffTime
  -> Bool
  -> SamplingFreqTable
  -> ChannelConfigTable
  -> (InitSegment, StreamingContext)
streamInitUtc !trackTitle !availabilityStartTime !segmentDuration !sbr !rate !channels =
  let !cfg = AacMp4StreamConfig t0 trackTitle sbr rate channels
      !t0  = utcToMp4 availabilityStartTime
      !dur = diffTimeToTicks segmentDuration timeScale
      !timeScale = getAacMp4StreamConfigTimeScale cfg
  in (InitSegment
      (BL.toStrict (toLazyByteString (buildAacMp4StreamInit cfg)))
     , StreamingContext cfg dur 0 0 [])

-- | Return the 'AacMp4StreamConfig' from an 'StreamingContext'
getStreamConfig :: StreamingContext -> AacMp4StreamConfig
getStreamConfig StreamingContext{..} = acConfig

-- | Return the current base decoding time
getStreamBaseTime :: StreamingContext -> Word64
getStreamBaseTime StreamingContext{..} = acBaseTime

-- | Return the current sequence number
getStreamSequence :: StreamingContext -> Word32
getStreamSequence StreamingContext{..} = acSequence

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

data Segment =
  Segment
  { segmentSequence :: !Word32
  , segmentTime     :: !Word64
  , segmentData     :: !BS.ByteString
  }

instance Show Segment where
  show (Segment !s !t !d) =
    printf "SEGMENT - seq: %14d - time: %14d - size: %14d" s t (BS.length d)


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
                   , acSegmentDuration :: !Word64 -- (in ticks)
                   , acSequence        :: !Word32
                   , acBaseTime        :: !Word64
                   , acSegments        :: ![(Word32, BS.ByteString)]
                   }

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

getSegmentDuration :: StreamingContext -> NominalDiffTime
getSegmentDuration !sc = segmentDuration
  where
    !segmentDuration = ticksToDiffTime (acSegmentDuration sc) timeScale
    !timeScale = getAacMp4StreamConfigTimeScale (acConfig sc)


-- | Initialisation segment parameters of an aac audio stream mp4 file.
data AacMp4StreamConfig =
  AacMp4StreamConfig { creationTime  :: !(TS32 "creation_time")
                     , trackName     :: !String
                     , useHeAac      :: !Bool
                     , sampleRate    :: !SamplingFreqTable
                     , channelConfig :: !ChannelConfigTable}

sampleCountDuration :: AacMp4StreamConfig -> Word32 -> Word64
sampleCountDuration (AacMp4StreamConfig _ _ _ _ c) r =
  fromIntegral r `div` channelConfigToNumber c

getAacMp4StreamConfigTimeScale :: AacMp4StreamConfig -> TimeScale
getAacMp4StreamConfigTimeScale AacMp4StreamConfig{..} =
  TimeScale (sampleRateToNumber sampleRate)

-- | Convert a 'AacMp4StreamConfig' record to a generic 'Boxes' collection.
buildAacMp4StreamInit
  :: AacMp4StreamConfig -> Builder
buildAacMp4StreamInit AacMp4StreamConfig{..} =
  let
    timeScaleForSampleRate = TimeScale (sampleRateToNumber sampleRate)
  in
    mediaBuilder dash $
    -- TODO must be iso5 for the way we use elementary stream descriptors
    fileTypeBox (FileType "iso5" 0 ["isom","iso5","dash","mp42"])
    :. skipBox (Skip (BS.pack [0x00,0x00,0x00,0x1d,0x6d,0x6f,0x62,0x69,0x00,0x00,0x00,0x15,0x76,0x69,0x6e,0x66,0x31,0x2e,0x31,0x2e,0x30,0x2d,0x37,0x37,0x36,0x2e,0x65,0x6c,0x36]))
    :| movie
        ( movieHeader
          (MovieHeader $
           V0 (creationTime
                :+ relabelScalar creationTime
                :+ def
                :+ TSv0 0)
            :+ def :+ def :+def :+ def :+ def :+ def :+ Custom (Scalar 2))
          :. track
            (trackHeader
              TrackInMovieAndPreview
              (TrackHeader $
                V0 (creationTime
                    :+ relabelScalar creationTime
                    :+ 1
                    :+ Constant
                    :+ 0) :+
                def)
              :| media
              (mediaHeader (MediaHeader $
                             V0 (creationTime
                                  :+ relabelScalar creationTime
                                  :+ timeScaleForSampleRate
                                  :+ TSv0 0)
                             :+ def)
                :. handler (namedAudioTrackHandler (T.pack trackName))
                :| mediaInformation
                ( soundMediaHeader (SoundMediaHeader def)
                  :. (dataInformation $: localMediaDataReference)
                  :| sampleTable
                  ((sampleDescription
                     $: audioSampleEntry 0 (aacAudioSampleEntrySimple
                                            useHeAac
                                            sampleRate
                                            channelConfig
                                            (Scalar 16))
                     :. timeToSample []
                     :. sampleToChunk []
                     :. fixedSampleSize 0 0
                     :| chunkOffset32 [])))))
            :| (movieExtends $: trackExtendsUnknownDuration 1 1))

-- | Media fragment segment parameters of an aac audio stream mp4 file.
data AacMp4TrackFragment =
  AacMp4TrackFragment { fragmentSampleSequence      :: !Word32
                      , fragmentBaseMediaDecodeTime :: !Word64
                      , fragmentSamples             :: ![(Word32, BS.ByteString)]
                      }

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
