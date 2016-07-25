module DashSpec (spec) where

import Test.Hspec
import Data.ByteString.IsoBaseFileFormat.Brands.Dash
import qualified Data.ByteString.Lazy as BL

spec :: Spec
spec =
  do describe "minimalIsobmff version 0" $
       do it "renders some output at all" $
            do createionTime <- mp4CurrentTime
               let ct :: Word32
                   ct = fromScalar createionTime
                   ct3 = (ct `shiftR` 24) .&. 255
                   ct2 = (ct `shiftR` 16) .&. 255
                   ct1 = (ct `shiftR` 8) .&. 255
                   ct0 = (ct `shiftR` 0) .&. 255
                   cts :: [Word8]
                   cts = fromIntegral <$> [ct3,ct2,ct1,ct0]
               let args = exampleDashFile createionTime
                   doc = mkDash args
                   rendered = BL.unpack $ packMediaFile doc
                   expected =
                     [0
                     ,0
                     ,0
                     ,32
                     ,102
                     ,116
                     ,121
                     ,112
                     ,105
                     ,115
                     ,111
                     ,53
                     ,0
                     ,0
                     ,0
                     ,0
                     ,105
                     ,115
                     ,111
                     ,109
                     ,105
                     ,115
                     ,111
                     ,53
                     ,100
                     ,97
                     ,115
                     ,104
                     ,109
                     ,112
                     ,52
                     ,50
                     ,0
                     ,0
                     ,0
                     ,224
                     ,109
                     ,111
                     ,111
                     ,118
                     ,0
                     ,0
                     ,0
                     ,108
                     ,109
                     ,118
                     ,104
                     ,100
                     ,0
                     ,0
                     ,0
                     ,0] ++
                     cts ++
                     [0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,1
                     ,95
                     ,144
                     ,0
                     ,1
                     ,95
                     ,144
                     ,0
                     ,1
                     ,0
                     ,0
                     ,1
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,1
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,1
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,64
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,255
                     ,255
                     ,255
                     ,255
                     ,0
                     ,0
                     ,0
                     ,108
                     ,116
                     ,114
                     ,97
                     ,107
                     ,0
                     ,0
                     ,0
                     ,92
                     ,116
                     ,107
                     ,104
                     ,100
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,1
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,1
                     ,95
                     ,144
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,1
                     ,0
                     ,0
                     ,0
                     ,0
                     ,1
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,1
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,64
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0
                     ,0,0,0,8,109,100,105,97]
               -- BL.writeFile "/tmp/xxx.mp4" (BL.pack rendered)
               rendered `shouldBe` expected

exampleDashFile
  :: U32 "creation_time" -> Dash 0
exampleDashFile creationTime =
  Dash
    (MovieHeader $
     V0 (creationTime :+ 0 :+ Default :+ durationFromSeconds Default 1) :+
     def)
    (TrackHeader $
     V0 (0 :+ 0 :+ 1 :+ Constant :+ durationFromSeconds Default 1) :+
     def)
