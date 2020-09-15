#!/bin/sh
rm -fr builddir workdir
git restore -- ../lib/Input.hs ../lib/Typecheck.hs ../src/Test.hs
