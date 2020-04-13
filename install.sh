#!/bin/bash

brew cask install supercollider
brew uninstall clozure-cl wget sdl2 sdl2_ttf sdl2_image zeromq
brew install clozure-cl wget sdl2 sdl2_ttf sdl2_image zeromq
rm -rf ~/quicklisp
wget https://beta.quicklisp.org/quicklisp.lisp
ccl64 --load ./quicklisp.lisp \
      --eval '(quicklisp-quickstart:install)'       \
      --eval '(ql:add-to-init-file)'                \
      --eval '(quit)'
wget https://common-lisp.net/project/asdf/archives/asdf.lisp
cp asdf.lisp `brew list clozure-cl | grep asdf.lisp`
wget https://github.com/supercollider/sc3-plugins/releases/download/Version-3.11.0-rc1/sc3-plugins-3.11.0-rc1-macOS-signed.zip
mkdir -p "~/Library/Application Support/SuperCollider/Extensions"
unzip sc3-plugins-3.11.0-rc1-macOS-signed.zip -d "~/Library/Application Support/SuperCollider/Extensions"
exec ./run.sh
