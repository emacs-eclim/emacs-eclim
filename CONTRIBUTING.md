# Eclim Contribution Guide

The project is under active development and we are always looking for
assistance.

## Quick Guide

1. Fork `eclim`
1. Install [Cask](https://github.com/cask/cask).
1. Create a topic branch: `git checkout -b my_branch`
1. Make your changes and update the `History.txt` file
1. Write
   [ERT](https://www.gnu.org/software/emacs/manual/html_node/ert/)
   tests,
   or [Buttercup](https://github.com/jorgenschaefer/emacs-buttercup)
   specification tests that exercize the functionality in your changes.
1. Ensure that your changes pass the lint tests and compile properly:
    ```sh
    make init
    make lint
    make compile
    ```

1. Ensure that your changes do not break any tests:
    ```sh
    make test
    make specs
    ```

    All tests must pass.

1. Push to your branch: `git push origin my_branch`
1. Rebase and squash commits
1. Issue a pull-request for your topic branch

# Continuous Integration

This package uses [Travis CI](https://travis-ci.org/)
and [coveralls.io](https://coveralls.zendesk.com/hc/en-us) to ensure
that new submissions do not break the package and adequate testing is
present.

Travis-CI ensures that new submissions pass the Lint tests and
compile without any errors. The *build* badge on this repository's
home page gives the current status of the build.

Coveralls.io ensures that the tests are performing adequate code
coverage. The *coverage* badge on this repository's home page gives
the current status in terms of a percentage. New commits to this
repository should not make this number decrease.
