# `esds` Box Contents

      00000027 65736473 00000000

         ES_Descriptor
         | length
      x 03 |   ES_ID
      x   19   |sdf
      x     0001|url
      b         0|ocr
      b          0|    stream priority
      b           0    | DecoderConfigDescriptor
      b            00000 | length
      x                 04 | objectype indication: Audio ISO/IEC 14496-3
      x                   11 |     stream-type: 0x5 AudioStream
      x                     40     |Upstream
      b                       000101|reserved
      b                             0|     BufferSize DB
      b                              1     |       maxBitrate
      x                               000000       |       avgBitrate
      x                                     00000000       | DecoderSpecificInfo
      x                                             00000000 | length
      x                                                     05 |
      x                                                       02
      x                                                            1   1   9   0
      =b                                                        00010 - audioObjectType: 2 AAC LC
      =b                                                             0011 - (=3) samplingFrequencyIndex: 48k
      =b                                                                 0010 - channelConfiguration: channel pair
      =b                                                                     0 - frameLenflag           \
      =b                                                                      0 - dependsOnCoreCoder     | GASpecificconfig
      =b                                                                       0 - coreCoderDelay       /
                                                             SLConfigDescrTag
                                                             | length: 1
      x                                                     06 | Predefined according to iso 14496-14 section 3.1.2
      x                                                       01 |
      x                                                         02
