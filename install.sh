#!/bin/bash

brew install clozure-cl
brew cask install supercollider
brew install wget
wget https://beta.quicklisp.org/quicklisp.lisp
ccl64 --load ./quicklisp.lisp \
      --eval '(quicklisp-quickstart:install)'       \
      --eval '(ql:add-to-init-file)'                \
      --eval '(quit)'

exec ./run.sh
