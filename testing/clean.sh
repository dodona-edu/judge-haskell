#!/bin/sh
rm -fr builddir workdir ../lib/Input.hs ../lib/Typecheck.hs ../lib/*_test.hs ../src/Test.hs
git checkout -- ../run
