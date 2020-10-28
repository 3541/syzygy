#!/usr/bin/env bash

mkdir -p _shake
ghc --make Shakefile.hs -rtsopts -threaded -with-rtsopts=-IO -outputdir=_shake -o _shake/build && _shake/build "$@"
