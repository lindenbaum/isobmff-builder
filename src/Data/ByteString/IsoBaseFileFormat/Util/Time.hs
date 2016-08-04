-- | Time and timing utilities.
module Data.ByteString.IsoBaseFileFormat.Util.Time
       (referenceTime, utcToMp4, mp4CurrentTime, durationFromSeconds,
        TimeScale, Timing)
       where

import Data.Time.Clock
import Data.Time.Calendar
import Data.Ratio
import Data.ByteString.IsoBaseFileFormat.Box
import Data.ByteString.IsoBaseFileFormat.Util.BoxFields
import Data.ByteString.IsoBaseFileFormat.Util.Versioned
import Data.ByteString.IsoBaseFileFormat.ReExports

-- * Absolute Dates
-- | According to the standard, fields with absolute dates and times are in
-- seconds since 1904/01/01 at midnight (UTC). This is this reference time.
referenceTime :: UTCTime
referenceTime =
  let startDay = fromGregorian 1904 1 1
      startTime = 0
  in UTCTime startDay startTime

-- | Convert a 'UTCTime' to a number of seconds since 'referenceTime'.
utcToMp4 :: Num t
         => UTCTime -> t
utcToMp4 u =
  let picoSecondsDiff = toRational $ diffUTCTime u referenceTime
      picoSecondsDiffNumerator = numerator picoSecondsDiff
      picoSecondsDiffDenominator = denominator picoSecondsDiff
      secondsSinceReferenceTime =
        div picoSecondsDiffNumerator picoSecondsDiffDenominator
  in fromIntegral secondsSinceReferenceTime

-- | Get the current time as number of seconds since 'referenceTime'
mp4CurrentTime :: Num t
               => IO t
mp4CurrentTime = utcToMp4 <$> getCurrentTime

-- * Time-Scale and Durations
-- | Default time-scale value
--   Based on history and tradition this value is @90000@.
--   MPEG-2 TS defines a single clock for each program, running at 27MHz. The
--   timescale of MPEG-2 TS Hint Tracks should be divisable by 90000.
type TimeScale = Template (U32 "timescale") 90000

-- | Utility function to convert seconds (Integers) to any 'Num' using a
-- 'TimeScale', Since 'Scalar' has a 'Num' instance this can be used to generate
-- @duration@ fields.
durationFromSeconds :: Num t
                    => TimeScale -> Integer -> t
durationFromSeconds timeScale seconds =
  let timeScaleI = fromIntegral $ fromScalar $ templateValue timeScale
  in timeScaleI * fromInteger seconds

-- | Time and timing information about a movie.
--
-- The creation/modification times are in seconds since midnight, Jan. 1, 1904,
-- in UTC time. Time scale declares the time coordinate system, it specifies the
-- number of time units that pass one second. The time coordinate system is used
-- by e.g. the duration field, which by the way contains the duration of the
-- longest track, if known, or simply the equivalent of 1s.
type Timing (version :: Nat) = Versioned TimingV0 TimingV1 version

type TimingV0 = TimingImpl (Scalar Word32)

type TimingV1 = TimingImpl (Scalar Word64)

type TimingImpl uint =
     uint "creation_time"
  :+ uint "modification_time"
  :+ TimeScale
  :+ uint "duration"
