#!/bin/bash

# for discussion and documentation of full freeze file vs freezing only index-state:
# see https://github.com/haskell/cabal/issues/8059#issuecomment-1076024522
# and https://github.com/haskell/cabal/pull/9984/files

set -e

rm -f cabal.project.freeze
cabal freeze
sed -i -e '/^constraints.*/d' -e '/^ .*/d'  cabal.project.freeze
cat cabal.project.freeze
