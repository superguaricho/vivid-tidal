#!/bin/bash
echo Testing vivid-tidal audio and runing Vivid-Tidal Haskell interpreter. . .
VIVIDVERSION=0.1.0.0
# PACK=vivid-tidal-${VIVIDVERSION}
PACK=vivid-tidal
PACKPATH=$HOME/.local/share/haskell/${PACK}
cd $PACKPATH
cabal repl --repl-options="-ghci-script ${PACK}.ghci" --repl-options=-Wno-missing-home-modules
