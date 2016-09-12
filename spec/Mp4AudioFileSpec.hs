{-# LANGUAGE OverloadedStrings #-}
module Mp4AudioFileSpec (spec) where -- TODO rename to mp4 audio init spec

import Test.Hspec
import Data.ByteString.Mp4.AudioFile
import Data.ByteString.IsoBaseFileFormat.ReExports
import Data.ByteString.IsoBaseFileFormat.Boxes
import Data.ByteString.IsoBaseFileFormat.Box
import Data.ByteString.IsoBaseFileFormat.Util.BoxFields
import Data.ByteString.IsoBaseFileFormat.Util.Time
import Data.ByteString.IsoBaseFileFormat.Util.Versioned
import qualified Data.ByteString.Lazy as BL
import Data.Text ()

spec :: Spec
spec =
  do describe "SingleAudioTrackInit version 0" $
       do it "renders some output at all" $
            do createionTime <- mp4CurrentTime
               let ct :: Word32
                   ct = fromScalar creationTime
                   ct3 = (ct `shiftR` 24) .&. 255
                   ct2 = (ct `shiftR` 16) .&. 255
                   ct1 = (ct `shiftR` 8) .&. 255
                   ct0 = (ct `shiftR` 0) .&. 255
                   cts :: [Word8]
                   cts = fromIntegral <$> [ct3,ct2,ct1,ct0]
               let args = exampleSingleTrackInit creationTime
                   doc = mkSingleTrackInit args
                   rendered = BL.unpack $ toLazyByteString $ doc
                   expected =
                     [
                      -- ftyp box
                      0,0,0,28,102,116,121,112,100,97,115,104,0,0,0,0
                      ,105,115,111,109,105,115,111,53,109,112,52,50
                      -- moov box
                     ,0,0,1,232,109 ,111 ,111 ,118
                      -- mvhd box
                     ,0 ,0 ,0 ,108 ,109 ,118 ,104 ,100 ,0 ,0 ,0 ,0]
                      ++ cts ++ [0 ,0 ,0 ,0 ,0
                     ,1 ,95 ,144 ,0 ,1 ,95 ,144 ,0 ,1 ,0 ,0 ,1 ,0 ,0 ,0
                     ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,1 ,0 ,0 ,0 ,0 ,0 ,0
                     ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,1 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0
                     ,0 ,0 ,0 ,0 ,0 ,0 ,64 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0
                     ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0
                     ,255 ,255 ,255 ,255
                     -- trak box
                     ,0 ,0 ,1,116
                     ,116 ,114 ,97 ,107
                     -- tkhd box
                     ,0 ,0 ,0 ,92
                     ,116 ,107 ,104 ,100
                     ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0
                     ,0 ,1 ,0 ,0 ,0 ,0 ,0 ,1 ,95 ,144 ,0 ,0
                     ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,1 ,0 ,0 ,0
                     ,0 ,1 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0
                     ,0 ,1 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0
                     ,0 ,64 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0
                     -- mdia box
                     ,0,0,1,16
                     ,109,100,105,97
                     ,0,0,0,32,109,100,104,100,0,0,0,0
                     ,0,0,0,0,0,0,0,0,0,1,95,144,0,0,0,0,16,181,0,0
                     -- hdlr box
                     ,0,0,0,44
                     ,104,100,108,114
                     ,0,0,0,0,0,0,0,0,115,111,117,110,0,0,0,0,0,0,0,0,0,0,0,0
                     ,72,101,108,108,111,32,119,111,114,108,100,0
                     -- minf box
                     ,0,0,0,188
                     ,109,105,110,102
                     ,0,0,0,16
                     ,115,109,104,100,0,0,0,0,0,0,0,0
                     -- dinf boimport Data.ByteString.IsoBaseFileFormat.Boxes
                     ,0,0,0,36
                     ,100,105,110,102
                     -- dref box
                     ,0,0,0,28
                     ,100,114,101,102
                     ,0,0,0,0,0,0,0,1
                     -- url  box
                     ,0,0,0,12
                     ,117,114,108,32
                     ,0,0,0,1
                     -- stbl
                     ,0,0,0,36,109,112,52,97
                     ,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,2,0,16,0,0,0,0
                     ,187,128,0,0
                     -- stts
                     ,0,0,0,16,115,116,116,115,0,0,0,0,0,0,0,0
                     -- stsc
                     ,0,0,0,16,115,116,115,99,0,0,0,0,0,0,0,0
                     -- stco
                     ,0,0,0,16,115,116,99,111,0,0,0,0,0,0,0,0
                     -- stsz
                     ,0,0,0,20,115,116,115,122,0,0,0,0,0,0,0,0,0,0,0,0]
               BL.writeFile "/tmp/isobmff-test-case-dash-spec.mp4" (BL.pack rendered)
               rendered `shouldBe`
                 expected

exampleSingleTrackInit :: U32 "creation_time" -> SingleAudioTrackInit
exampleSingleTrackInit creationTime =
  SingleAudioTrackInit
                  (MovieHeader $
                   V0 (creationTime :+ 0 :+ Template :+ durationFromSeconds Template 1) :+
                   def)
                  (TrackHeader $
                   V0 (0 :+ 0 :+ 1 :+ Constant :+ durationFromSeconds Template 1) :+
                   def)
                  (MediaHeader def)
                  (namedAudioTrackHandler "Hello world")
                  (SoundMediaHeader def)
