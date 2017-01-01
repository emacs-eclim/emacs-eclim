# Emacs-eclim Contribution Guide

The project is under active development and we are always looking for
assistance.

## Quick Guide

1. Fork emacs-eclim
2. Create a topic branch - `git checkout -b my_branch`
3. Make your changes and update the History.txt file
4. Push to your branch - `git push origin my_branch`
5. Rebase and squash commits
6. Issue a pull-request for your topic branch
7. That's it!

## Tips

We use [Cask](https://github.com/cask/cask) to compile and test the
project. Install all dependencies with `cask install` and use `cask exec
ert-runner` to run all tests with the apropriate development dependencies.

Before submitting a pull request, ensure that `make lint` does not
report any errors.
