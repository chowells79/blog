#! /usr/bin/env sh

set -x
set -e

cabal update

cabal clean
rm cabal.project.freeze
cabal build all
cabal freeze

cabal clean
rm ci.project.freeze
cabal --project-file=ci.project build all --only-dependencies
cabal --project-file=ci.project freeze

cabal clean
