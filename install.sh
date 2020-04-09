#!/bin/bash

brew install clozure-cl
brew cask install supercollider
brew install wget
brew install sdl2
brew install sdl2_ttf
brew install sdl2_image
wget https://beta.quicklisp.org/quicklisp.lisp
ccl64 --load ./quicklisp.lisp \
      --eval '(quicklisp-quickstart:install)'       \
      --eval '(ql:add-to-init-file)'                \
      --eval '(quit)'

exec ./run.sh
