#!/bin/bash
cabal sandbox delete
cabal sandbox init
cabal install cabal-install && cabal install --only-dependencies --allow-newer=template-haskell,ListLike,hashtables
