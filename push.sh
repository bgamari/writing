#!/usr/bin/env bash

rm -Rf _site _cache
rm -f result result-*
nix build

cp -R result/* github
cd github
git add .
git commit -a -m "Automatic push"
git push
cd ..

rsync -av ./result/ ben@smart-cactus.org:public_html
