#!/bin/bash

if [ $# != 1 ]; then
    echo "Usage: $0 <ghc-install-path>"
    exit 1
fi

sudo ln -sf $1/bin/ghc /usr/bin/ghc
sudo ln -sf $1/bin/ghci /usr/bin/ghci
sudo ln -sf $1/bin/haddock /usr/bin/haddock
sudo ln -sf $1/bin/runhaskell /usr/bin/runhaskell
sudo ln -sf $1/bin/hsc2hs /usr/bin/hsc2hs
sudo ln -sf $1/bin/hp2ps /usr/bin/hp2ps
sudo ln -sf $1/bin/ghc-pkg /usr/bin/ghc-pkg
