# judge-haskell

## Testing

`tests/exercises/` holds small exercises, each with student submissions and the
feedback the judge is expected to give on them:

```
tests/exercises/<exercise>/
  evaluation/              resources, as in a real exercise
  workdir/                 optional, copied to the working directory
  config.json              optional, merged into the judge config
  submissions/<case>.hs    <case>.en.hs runs with natural_language "en", others with "nl"
  expected/<case>.json     exit code, stderr and feedback of the judge
```

`tests/run-exercises.sh` runs the judge on every submission the way Dodona does
and diffs the result with the expected file. Run it inside the judge image:

```sh
devel/run-tests-in-image.sh                      # all cases
devel/run-tests-in-image.sh hunit/wrong          # cases whose <exercise>/<case> contains the filter
UPDATE_EXPECTED=1 devel/run-tests-in-image.sh    # rewrite the expected files
IMAGE=... devel/run-tests-in-image.sh            # another image than ghcr.io/dodona-edu/dodona-haskell:latest
```

Review the changes to `expected/` after an update: they are the judge's
feedback, bugs included.

CI runs the suite on every push in `ghcr.io/dodona-edu/dodona-haskell:latest`
(`.github/workflows/integration_test.yml`). `.github/workflows/e2e.yml` takes
the image and judge ref as inputs, so docker-images can verify a new image
against the suite before publishing it.
