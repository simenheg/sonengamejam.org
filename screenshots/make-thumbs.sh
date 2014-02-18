#!/bin/sh

mkdir -p thumbs

for f in *.png
  do convert $f -thumbnail 240 thumbs/$f
done
