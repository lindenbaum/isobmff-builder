
# Contents of a `trun` box


                                            0000 047c
    00000060: 7472 756e 0000 0701 0000 005e 0000 04c8
    00000070: 0000 0400 0000 0095 0200 0000 0000 0400
    00000080: 0000 0076 0200 0000 0000 0400 0000 008c
    00000090: 0200 0000 0000 0400 0000 008b 0200 0000
    000000a0: 0000 0400 0000 0094 0200 0000 0000 0400
    000000b0: 0000 007a 0200 0000 0000 0400 0000 0087
    000000c0: 0200 0000 0000 0400 0000 0099 0200 0000
    000000d0: 0000 0400 0000 0095 0200 0000 0000 0400

## Box header

    0x0000047c        size
    0x7472 756e       'trun'
    0x00              version
    0x000701          flags
    0x0000005e        sample_count = 94
    0x000004c8        data_offset

## A closer look at a single entry

    0x00000400        sample duration
    0x00000095        sample size
    0x02000000        sample flags


### Sample flags

    0    2    0    0    0    0    0    0      (hex)
    0000 0010 0000 0000 0000 0000 0000 0000   (binary)

    0000 -> 4 bits reserved = 0
         00 -> is_leading
           10 -> sample_depends
              00 -> sample_is_depended_on
                00 -> sample has redundancy
                   000 -> padding
                      0 -> non sync
                        0000 0000 0000 0000  degradation priority
