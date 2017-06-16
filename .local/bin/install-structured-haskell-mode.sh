#!/bin/bash
set -e
set -x

# Test if there's any cabal
which cabal

cd ~/.emacs.d/
git clone https://github.com/chrisdone/structured-haskell-mode.git

# You need to install the structured-haskell-mode executable which does the parsing.
cd structured-haskell-mode
cabal install
cd elisp/
make
