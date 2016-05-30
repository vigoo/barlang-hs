#!/bin/bash

set -euf -o pipefail

npm install jquery
npm install ansi_up

stack build

cp -rv ace .stack-work/install/x86_64-osx/lts-3.8/ghcjs-0.2.0_ghc-7.10.3/bin/barlang-webapp.jsexe/ace
cp -v node_modules/ansi_up/ansi_up.js .stack-work/install/x86_64-osx/lts-3.8/ghcjs-0.2.0_ghc-7.10.3/bin/barlang-webapp.jsexe/
cp -v node_modules/jquery/dist/jquery.min.js .stack-work/install/x86_64-osx/lts-3.8/ghcjs-0.2.0_ghc-7.10.3/bin/barlang-webapp.jsexe/
cp -v barlang.css .stack-work/install/x86_64-osx/lts-3.8/ghcjs-0.2.0_ghc-7.10.3/bin/barlang-webapp.jsexe/
cp -v barlang.js .stack-work/install/x86_64-osx/lts-3.8/ghcjs-0.2.0_ghc-7.10.3/bin/barlang-webapp.jsexe/
cp -v barlang.html .stack-work/install/x86_64-osx/lts-3.8/ghcjs-0.2.0_ghc-7.10.3/bin/barlang-webapp.jsexe/index.html

open .stack-work/install/x86_64-osx/lts-3.8/ghcjs-0.2.0_ghc-7.10.3/bin/barlang-webapp.jsexe/index.html
