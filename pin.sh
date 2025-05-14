#!/bin/bash

set -e

rm -f cabal.project.freeze
cabal freeze
sed -i -e '/^constraints.*/d' -e '/^ .*/d'  cabal.project.freeze
cat cabal.project.freeze
