{-# LANGUAGE OverloadedStrings #-}
module Mp4AudioSegmentSpec (spec) where

import           Data.ByteString.IsoBaseFileFormat.ReExports
import           Data.ByteString.IsoBaseFileFormat.Util.BoxFields
import           Data.ByteString.IsoBaseFileFormat.Util.Time
import qualified Data.ByteString.Lazy                             as BL
import           Data.ByteString.Mp4.AudioFile
import           Data.ByteString.Mp4.Boxes.AudioSpecificConfig
import           Data.ByteString.Mp4.Boxes.BaseDescriptor
import           Data.ByteString.Mp4.Boxes.Mp4AudioSampleEntry
import           Data.Text                                        ()
import           Test.Hspec

spec :: Spec
spec =
  do describe "AacMp4StreamFragment" $
       do it "renders some output at all" $
            do let args = moof
                   doc = buildAacMp4StreamFragment args
                   rendered = BL.unpack $ toLazyByteString $ doc
                   expected =
                     [
                      -- styp box
                       0,0,0,32
                     ,115,116,121,112
                     ,105,115,111,53,0,0,0,0
                     ,105,115,111,109
                     ,105,115,111,53
                     ,100,97,115,104
                     ,109,112,52,50
                     -- moov box
                     ,0,0,0,64
                     ,109,111,111,102
                     -- mfhd box
                     ,0,0,0,16
                     ,109,102,104,100
                     ,0,0,0,0,0,0,5,57
                     -- traf
                     ,0,0,0,40
                     ,116,114,97,102
                     -- tfhd
                     ,0,0,0,16
                     ,116,102,104,100
                     ,0,2,0,0,0,0,0,1
                     -- tfdt
                     ,0,0,0,16
                     ,116,102,100,116
                     ,0,0,0,0,0,1,17,42]

               BL.writeFile "/tmp/isobmff-test-case-dash-spec.m4s" (BL.pack rendered)
               rendered `shouldBe`
                 expected

moof :: AacMp4StreamFragment
moof =
  AacMp4StreamFragment 1337 777
