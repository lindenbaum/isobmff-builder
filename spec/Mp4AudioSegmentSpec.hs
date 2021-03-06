{-# LANGUAGE OverloadedStrings #-}
module Mp4AudioSegmentSpec (spec) where

import qualified Data.ByteString                             as BS
import           Data.ByteString.IsoBaseFileFormat.ReExports
import qualified Data.ByteString.Lazy                        as BL
import           Data.ByteString.Mp4.AudioStreaming
import           Data.Text                                   ()
import           Test.Hspec

spec :: Spec
spec =
  describe "AacMp4TrackFragment" $
   do let args = moof
          doc = buildAacMp4TrackFragment args
          rendered = BL.unpack $ toLazyByteString $ doc
          dataOffset = 120
          expected =
                     [
                      -- styp box
                       0,0,0,24
                     ,115,116,121,112
                     ,109,115,100,104,0,0,0,0
                     ,109,115,100,104
                     ,100,97,115,104
                     -- moov box [offset here: 32]
                     ,0,0,0,112
                     ,109,111,111,102
                     -- mfhd box
                     ,0,0,0,16
                     ,109,102,104,100
                     ,0,0,0,0,0,0,0,13
                     -- traf
                     ,0,0,0,88
                     ,116,114,97,102
                     -- tfhd
                     ,0,0,0,16
                     ,116,102,104,100
                     ,0,2,0,0,0,0,0,1
                     -- tfdt
                     ,0,0,0,20
                     ,116,102,100,116
                     ,1,0,0,0,0,0,0,0,0,0,0,37
                     -- trun
                     ,0,0,0,44
                     ,116,114,117,110
                     ,0,0,7,1
                     ,0,0,0,2
                     ,0,0,0,dataOffset
                     -- sample 1
                     ,0,0,0,23 -- duration
                     ,0,0,0,192 -- length
                     ,2,0,0,0 -- flags
                     -- sample 2
                     ,0,0,0,23
                     ,0,0,0,192
                     ,2,0,0,0
                     -- mdat
                     ,0,0,1,136
                     ,109,100,97,116]
                     -- sample 1
                     ++ [0..191]
                     -- sample 2
                     ++ [0..191]
      it "renders the exact expectected output" $ do
#ifdef COMPLEXTESTS
        BL.writeFile "/tmp/isobmff-test-case-dash-spec.m4s" (BL.pack rendered)
#endif
        rendered `shouldBe` expected
      it "calculates the data-offset correctly" $
        drop ((fromIntegral dataOffset) + 12 * 2) rendered
        `shouldBe` ([0..191] ++ [0..191])

moof :: AacMp4TrackFragment
moof = AacMp4TrackFragment 13 37 (replicate 2 (23, BS.pack [0..191]))
