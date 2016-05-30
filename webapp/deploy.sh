#!/bin/bash

set -euf -o pipefail

target=/tmp/barlang-webapp
git clone git@github.com:vigoo/barlang.git $target
pushd $target
git checkout gh-pages
git pull

popd
stack build

cp -rv ace $target/ace
cp -v node_modules/ansi_up/ansi_up.js $target/
cp -v node_modules/jquery/dist/jquery.min.js $target/
cp -v barlang.css $target/
cp -v barlang.js $target/
cp -v barlang.html $target/index.html

pushd $target
git commit -a -m "barlang-webapp update"
git push
popd
