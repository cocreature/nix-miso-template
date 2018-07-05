#!/bin/sh
ALL_JS=`cabal-plan list-bins | awk '{ print $2 ".jsexe/all.js" }'`
ln -s "$ALL_JS" static/all.js
