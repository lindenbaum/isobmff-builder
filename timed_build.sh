#!/bin/bash

set -xe


stack clean
stack build --dependencies-only

( time stack build ) 2>&1 | tee $(git rev-list -n1 --abbrev HEAD)-build-timed.log
