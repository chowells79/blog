#!/usr/bin/env bash

set -e

./scripts/google-font-download/google-font-download -f woff,woff2\
    "Roboto" "Roboto Mono"
./scripts/google-font-download/google-font-download -f woff,woff2\
    "Material Symbols Sharp" "Material Symbols Outlined" "Material Symbols Rounded"

mv *.woff *.woff2 content/fonts
rm font.css
