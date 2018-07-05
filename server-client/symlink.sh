#!/bin/sh
mkdir -p static
JS_EXE=`cabal-plan list-bins | awk '{ print $2 ".jsexe" }'`
for file in "rts.js" "lib.js" "out.js" "runmain.js" "index.html"; do
    ln -s "$JS_EXE/$file" "static/$file"
done
