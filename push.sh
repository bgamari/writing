#!/usr/bin/env bash

rm -f result
nix build

cp -R result/* github
cd github
git add .
git commit -a -m "Automatic push"
git push
cd ..

rsync result/ ben@smart-cactus.org:public_html -a
