{-# LANGUAGE OverloadedStrings #-}
module ElementaryStreamDescriptorSpec (spec) where

import Test.Hspec
import Data.ByteString.Mp4.Boxes.ElementaryStreamDescriptor
import Data.ByteString.Mp4.Boxes.AudioSpecificConfig
import Data.ByteString.Mp4.Boxes.DecoderConfigDescriptor
import Data.ByteString.Mp4.Boxes.DecoderSpecificInfo
import Data.ByteString.IsoBaseFileFormat.ReExports
import Data.ByteString.IsoBaseFileFormat.Box

spec :: Spec
spec = do
#ifndef COMPLEXTESTS
  return ()
#else
  describe "EsdBox" $ do
    let eb = esdBox (Proxy @TestEsDescriptor) False 0 0 0
    it "has the correct size" $
      boxSize eb `shouldBe` 39
    it "generates the expected bits" $
      printBuilder (boxBuilder eb)
      `shouldBe` "<< 00 00 00 27 65 73 64 73 00 00 00 00 03 19 00 01 00 04 11 40 15 00 00 00 00 00 00 00 00 00 00 00 05 02 11 98 06 01 02 >>"

type TestEsDescriptor  =
  ESDescriptorMp4File
   (StaticFieldValue "esId" 1)
   TestConfigDescriptor

type TestConfigDescriptor  =
  DecoderConfigDescriptor
  'AudioIso14496_3
  'AudioStream
  '[AudioConfigAacMinimal
     'AacLc
     DefaultGASpecificConfig
     (SetEnum "samplingFreq" SamplingFreq 'SF48000)
     (SetEnum "channelConfig" ChannelConfig 'SinglePair)]
  '[]
#endif
