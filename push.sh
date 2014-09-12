#!/bin/bash

./hakyll build

cp -R _site/* github
cd github
git add .
git commit -a -m "Automatic push"
git push
cd ..

rsync _site/ smart-cactus.org:public_html -a
