#!/bin/bash
cabal install cabal-install && cabal install --only-dependencies --allow-newer=template-haskell,ListLike,hashtables
