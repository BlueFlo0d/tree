#!/bin/bash

brew cask install supercollider
brew uninstall clozure-cl wget sdl2 sdl2_ttf sdl2_image
brew install clozure-cl wget sdl2 sdl2_ttf sdl2_image
rm -rf ~/quicklisp
wget https://beta.quicklisp.org/quicklisp.lisp
ccl64 --load ./quicklisp.lisp \
      --eval '(quicklisp-quickstart:install)'       \
      --eval '(ql:add-to-init-file)'                \
      --eval '(quit)'
wget https://common-lisp.net/project/asdf/archives/asdf.lisp
cp asdf.lisp `brew list clozure-cl | grep asdf.lisp`
exec ./run.sh
