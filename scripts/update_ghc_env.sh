#!/bin/bash

GHC_FILES=`ls -lah /usr/bin/ | grep ghc | tr -s '[[:space:]]' | cut -d " " -f 9`


for file in $GHC_FILES; do
    sudo ln -fs /opt/ghc-7.10.2/bin/$file /usr/bin/$file;
done
