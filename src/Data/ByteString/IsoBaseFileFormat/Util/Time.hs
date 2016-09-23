-- | Time and timing utilities.
module Data.ByteString.IsoBaseFileFormat.Util.Time
       (referenceTime, utcToMp4, mp4CurrentTime, durationFromSeconds,
        oneSecond32, oneSecond64, diffTimeToTicks,ticksToDiffTime,
        TimeScale(..), Timing, TS(..), type TS32, type TS64, type Ticks(..))
       where

import Data.Time.Clock
import Data.Time.Calendar
import Data.Ratio
import Data.ByteString.IsoBaseFileFormat.Box
import Data.ByteString.IsoBaseFileFormat.Util.BoxFields
import Data.ByteString.IsoBaseFileFormat.Util.Versioned
import Data.ByteString.IsoBaseFileFormat.ReExports
import Data.Typeable
import Foreign.Storable

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


-- | Convert a 'NominalDiffTime' to the number of 'Ticks' with respect
-- to a given 'TimeScale'.
diffTimeToTicks :: Integral t
                => NominalDiffTime -> TimeScale -> t
diffTimeToTicks !diff (TimeScale !scale) =
  round (diff * (fromIntegral scale))

-- | Convert a 'NominalDiffTime' to the number of 'Ticks' with respect
-- to a given 'TimeScale'.
ticksToDiffTime :: Integral t
                => t -> TimeScale -> NominalDiffTime
ticksToDiffTime !t (TimeScale !scale) =
  fromRational (toInteger t % toInteger scale)


-- | Get the current time as number of seconds since 'referenceTime'
mp4CurrentTime :: Num t
               => IO t
mp4CurrentTime = utcToMp4 <$> getCurrentTime

-- * Time-Scale and Durations
-- | Default time-scale value
--   Based on history and tradition this value is @90000@.
--   MPEG-2 TS defines a single clock for each program, running at 27MHz. The
--   timescale of MPEG-2 TS Hint Tracks should be divisable by 90000.
newtype TimeScale = TimeScale {fromTimeScale :: Word32}
  deriving (Show,Eq,Num,Bounded,Ord,Bits,Integral,Typeable,Storable,Enum,Real)

instance Default TimeScale where
  def = TimeScale 90000

instance IsBoxContent TimeScale where
  boxSize = boxSize . fromTimeScale
  boxBuilder = boxBuilder . fromTimeScale

-- | Utility function to convert seconds (Integers) to any 'Num' using a
-- 'TimeScale', Since 'Scalar' has a 'Num' instance this can be used to generate
-- @duration@ fields.
durationFromSeconds :: Num t
                    => TimeScale -> Integer -> t
durationFromSeconds timeScale seconds =
  let timeScaleI = fromIntegral timeScale
  in timeScaleI * fromInteger seconds

-- | Utility function to generate the equivalent of one second (@1 s@)
oneSecond32 = Scalar . flip durationFromSeconds 1
oneSecond32 :: TimeScale -> TS32 label

-- | Utility function to generate the equivalent of one second (@1 s@)
oneSecond64 :: TimeScale -> TS64 label
oneSecond64 = Scalar . flip durationFromSeconds 1

-- | A type that denotes a time relative to a 'TimeScale' which is included in
-- its type. 'Ticks' is the number of time units passed, where each time unit
-- has a physical duration of @timescale * 1/s@ i.e. @timescale@ 'Ticks' last
-- about @1 s@.
-- TODO use this instead of raw Word32s
newtype Ticks (perSecond :: Nat) = Ticks {fromTicks :: Word32}
  deriving (Show,Eq,Num,Bounded,Ord,Bits,Integral,Typeable,Storable,Enum,Real)

instance IsBoxContent (Ticks n) where
  boxSize = boxSize . fromTicks
  boxBuilder = boxBuilder . fromTicks

-- | Time and timing information about a movie.
--
-- The creation/modification times are in seconds since midnight, Jan. 1, 1904,
-- in UTC time. Time scale declares the time coordinate system, it specifies the
-- number of time units that pass one second. The time coordinate system is used
-- by e.g. the duration field, which by the way contains the duration of the
-- longest track, if known, or simply the equivalent of 1s.
type Timing (version :: Nat) = Versioned TimingV0 TimingV1 version

type TS32 = Scalar Word32
type TS64 = Scalar Word64

data TS (version :: Nat) (label :: Symbol) where
  TSv0 :: !Word32 -> TS 0 label
  TSv1 :: !Word64 -> TS 1 label

instance IsBoxContent (TS v n) where
  boxSize (TSv0 _t) = 4
  boxSize (TSv1 _t) = 8
  boxBuilder (TSv0 !t) = word32BE t
  boxBuilder (TSv1 !t) = word64BE t

type TimingV0 = TimingImpl (Scalar Word32) (TS 0 "duration")

type TimingV1 = TimingImpl (Scalar Word64) (TS 1 "duration")

type TimingImpl uint dur =
     uint "creation_time"
  :+ uint "modification_time"
  :+ TimeScale
  :+ dur
