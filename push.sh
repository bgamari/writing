#!/bin/bash

runghc hakyll.hs build
cp -R _site/* github
cd github
git add .
git commit -a -m "Automatic push"
git push

