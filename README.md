# judge-haskell

Dodona starts `run`, which compiles the driver `src/Judge.hs` and runs it with the submission config on stdin. The driver compiles `lib/` with the submission, then compiles and runs each test file, and writes the feedback around what the tests write themselves. Everything in `lib/` is compiled alongside the submission and visible to test files; the driver's own modules stay in `src/`. The driver lints the submission with the `hlint` executable the image installs in the runner's `~/.local/bin` and turns its `--json` output into `annotate-code` commands (`src/Lint.hs`).

The texts the judge shows students are in `lib/JudgeMessages.hs`, in Dutch and English, picked by the config's `natural_language` (English for anything but `nl`). The driver passes the language on to the test programs in `DODONA_NATURAL_LANGUAGE`.

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

A case may have `expected/<case>.ghc-<major.minor>.json` for one GHC version, used instead of the default when the image runs that version. It exists while the published image and a candidate run different GHC versions, and is folded into the default once the new image is published.

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

### Dependencies

The judge image owns the runtime dependencies; `packages.txt` is a checked copy of its library list.

```sh
devel/install_deps.sh                    # GHC package environment with packages.txt
devel/install_deps.sh packages-test.txt  # the same, plus the unit test libraries
devel/install_dev_deps.sh                # hlint, into cabal's installdir
```

### Unit tests and lint

`knownBug "#N ..."` asserts the intended behaviour for a filed bug: the example
is pending while the bug is there and fails once it is fixed, so the marker gets
removed with the fix.

```sh
devel/run-unit-tests.sh      # after devel/install_deps.sh packages-test.txt; arguments go to hspec
devel/check.sh               # hlint on lib/, src/ and tests/unit/, after devel/install_dev_deps.sh
```

`test.yml` builds a fresh environment from the lists and runs the unit tests and lint; `packages.yml` (`packages-match-image`) checks `packages.txt` and `.tool-versions` against the image.
