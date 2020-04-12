#!/bin/bash

killall scsynth
ccl64 --eval "(defvar *cli-player-id* $1)" \
      --load ./load.lisp
