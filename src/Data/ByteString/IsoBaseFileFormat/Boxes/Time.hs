-- | Time and timing utilities.
module Data.ByteString.IsoBaseFileFormat.Boxes.Time
      -- (mp4ToUTCTimeU64, utcTimeToMp4U64, mp4CurrentTimeU64
      -- ,mp4ToUTCTime, , mp4CurrentTime)
      (referenceTime,utcToMp4,mp4CurrentTime,durationFromSeconds,TimeScale)
      where

import Data.Time.Clock
import Data.Time.Calendar
import Data.Ratio
import Data.ByteString.IsoBaseFileFormat.Boxes.BoxFields

-- * Absolute Dates

-- | According to the standard, fields with absolute dates and times are in
-- seconds since 1904/01/01 at midnight (UTC). This is this reference time.
referenceTime :: UTCTime
referenceTime =
  let startDay = fromGregorian 1904 1 1
      startTime = 0
  in UTCTime startDay startTime

-- | Convert a 'UTCTime' to a number of seconds since 'referenceTime'.
utcToMp4 :: Num t => UTCTime -> t
utcToMp4 u =
  let picoSecondsDiff = toRational $ diffUTCTime u referenceTime
      picoSecondsDiffNumerator = numerator picoSecondsDiff
      picoSecondsDiffDenominator = denominator picoSecondsDiff
      secondsSinceReferenceTime =
        div picoSecondsDiffNumerator picoSecondsDiffDenominator
  in fromIntegral secondsSinceReferenceTime

-- | Get the current time as number of seconds since 'referenceTime'
mp4CurrentTime :: Num t => IO t
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
durationFromSeconds :: Num t => TimeScale -> Integer -> t
durationFromSeconds timeScale seconds =
  let timeScaleI = fromIntegral $ fromScalar $ templateValue timeScale
  in  timeScaleI * fromInteger seconds
