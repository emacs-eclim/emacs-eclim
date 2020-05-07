[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)
[![Build Status](https://travis-ci.org/emacs-eclim/emacs-eclim.svg?branch=master)](https://travis-ci.org/emacs-eclim/emacs-eclim)
[![Coverage Status](https://coveralls.io/repos/github/emacs-eclim/emacs-eclim/badge.svg?branch=master)](https://coveralls.io/github/emacs-eclim/emacs-eclim?branch=master)
[![MELPA](http://melpa.org/packages/eclim-badge.svg)](http://melpa.org/#/eclim)
[![MELPA Stable](http://stable.melpa.org/packages/eclim-badge.svg)](http://stable.melpa.org/#/eclim)
[![Open Source Helpers](https://www.codetriage.com/emacs-eclim/emacs-eclim/badges/users.svg)](https://www.codetriage.com/emacs-eclim/emacs-eclim)

# No Longer Maintained

This package is no longer maintained.

A better Java programming environment can be set up using [lsp-java](https://github.com/emacs-lsp/lsp-java). Please consider using it instead.

# Overview

[![Join the chat at https://gitter.im/emacs-eclim/emacs-eclim](https://badges.gitter.im/emacs-eclim/emacs-eclim.svg)](https://gitter.im/emacs-eclim/emacs-eclim?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

[Eclim](http://eclim.org) is an Eclipse plugin which exposes Eclipse
features through a server interface. When this server is started, the
command line utility `eclim` can be used to issue requests to that
server.

The eclim package uses the Eclim Server to integrate Eclipse with
Emacs. This project wants to bring some of the invaluable features
from Eclipse to Emacs. Please note, the eclim package **is limited to
mostly Java support at this time.**

It is also possible to start and stop the eclim daemon from Emacs using the
`eclimd` package.

You can ask questions or discuss new features on the Gitter channel
(see badge above), and at
our
[Google Group](https://groups.google.com/forum/#!forum/emacs-eclim)

## Package renamed

This package was originally called *emacs-eclim*, and was renamed to
*eclim* on August 15, 2016 because `use-package` could not load it from
the [MELPA][melpa] package archive.

# A note about Eclim versions

Prior to version 1.7.3, Eclim used a proprietary protocol for
communication with the Eclim Server. If you are running one of these
older versions, you need version 0.1 of this package.

Eclim versions 1.7.3 and later however, serves responses using a
standard JSON format. These are supported by versions 0.2
and later of this package.

The eclim package versions are tagged with the appropriate version
number. You can see and download previous
releases [here](https://github.com/emacs-eclim/emacs-eclim/tags).

# Installation

emacs-eclim requires Emacs version 24.5 or later.

1. [Download and install](http://eclim.org/install.html) eclim.
1. Install emacs-eclim. You have two options:
   * Installation from the [MELPA][melpa] package archive. Just
     [add the archive to `package-archives`](https://melpa.org/#/getting-started)
     if you haven't already, and then install the "eclim" package with the
     `package-install` command.
   * Manual installation from GitHub.
       1. (`git clone git://github.com/emacs-eclim/emacs-eclim.git`)
       1. Add `(add-to-list 'load-path "/path/to/emacs-eclim/")` to your startup
          script.
       1. Make sure all dependencies are available, see
          [`eclim-pkg.el`](eclim-pkg.el).
1. Add the following code to your emacs startup script:

    ```emacs-lisp
    (require 'eclim)
    (setq eclimd-autostart t)

    (defun my-java-mode-hook ()
        (eclim-mode t))

    (add-hook 'java-mode-hook 'my-java-mode-hook)
    ```
    Or, if you prefer to enable eclim-mode globally:

    ```emacs-lisp
    (require 'eclim)
    (setq eclimd-autostart t)
    (global-eclim-mode)
    ```

    If you wish to start
    the [Eclim Server](http://eclim.org/#how-do-i-get-install-it)
    outside of Emacs, then set `eclimd-autostart` to `nil`.

Now every time you open a file that belongs to a Eclipse project eclim mode is
enabled.

# Configuration

## Eclipse installation

Emacs-eclim tries its best to locate your Eclipse installation. If you
have Eclipse installed in a non-standard location (i.e.
`~/nonStandard/eclipse` or `/opt/eclipse`) you may specify the paths
manually by adding this to your startup script:

```emacs-lisp
(custom-set-variables
  '(eclim-eclipse-dirs '("~/nonStandard/eclipse"))
  '(eclim-executable "~/nonStandard/eclipse/eclim"))
```

## Displaying compilation error messages in the echo area

When the cursor is positioned on an error marker in a code buffer,
emacs-eclim uses the local help feature in emacs to display the
corresponding error message in the echo area. You can either invoke
`(display-local-help)` manually or activate automatic display of local
help by adding the following to .emacs:

```emacs-lisp
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)
```

## Configuring auto-complete-mode

If you wish to use [auto-complete-mode] with emacs-eclim, add the
following to your .emacs:

```emacs-lisp
;; regular auto-complete initialization
(require 'auto-complete-config)
(ac-config-default)

;; add the emacs-eclim source
(require 'ac-emacs-eclim-source)
(ac-emacs-eclim-config)
```

## Configuring company-mode

Emacs-eclim can integrate with [company-mode] to provide pop-up
dialogs for auto-completion. To activate this, you need to add the
following to your .emacs:

```emacs-lisp
(require 'company)
(require 'company-emacs-eclim)
(company-emacs-eclim-setup)
(global-company-mode t)
```

Emacs-eclim completions in company are case sensitive by default. To
make completions case insensitive set
`company-emacs-eclim-ignore-case` to `t`.

If you installed Eclim from [MELPA](melpa) you will have to install
`company-emacs-eclim` as well. Add the archive to package-archives if
you haven't already, and then install the `company-emacs-eclim`
package with the `package-install` command.

## Configuring eclimd module

When `emacs-eclim` is configured correctly, you don't need to modify the
configuration for the `eclimd` package. Still, in the customization group
`eclimd` there are a few variables you can tweak.

# Usage

To use `eclim-mode`'s features `eclimd` must to be running (`M-x eclimd-start`
or set `eclimd-autostart` to `t`) and the files you are editing have to be
organized in a Eclipse project (`M-x eclim-project-create`).

* [Projects](http://wiki.github.com/emacs-eclim/emacs-eclim/projects)
* [Code Completion](http://wiki.github.com/emacs-eclim/emacs-eclim/code-completion)
* [Java](http://wiki.github.com/emacs-eclim/emacs-eclim/java)
* [Ant](http://wiki.github.com/emacs-eclim/emacs-eclim/ant)
* [Maven](http://wiki.github.com/emacs-eclim/emacs-eclim/maven)
* [Problems and Errors](http://wiki.github.com/emacs-eclim/emacs-eclim/problems-and-errors)

## Starting eclimd

Since most of eclim's commands require eclimd, you should ensure eclimd is
running when you need it. Here are your options:

* Variable `eclimd-autostart`: Enables automatic starting of eclimd whenever
  `eclim-mode` is enabled or when `global-eclim-mode` needs it.
* Use the commands `start-eclimd` and `stop-eclimd`.
* Start `eclimd` manually using Eclipse or a shell. If you always want to start
  eclimd like this you should add `(setq eclimd-autostart nil)` to your startup
  script to prevent accidental starting of eclimd from within Emacs.

# Compiling this package

This package uses [Cask](https://github.com/cask/cask) to compile and
test the project. Please install it prior to compiling the files.
Package dependencies are installed using the `make init` command in a
shell. Once dependencies have been installed, use `make compile` to
compile the files.

# Press

Read more about emacs-eclim:

* [Enterprise Java Development in Emacs](http://www.skybert.net/emacs/java/), \[Torstein Krause Johansen\]
* [The Ballad of Emacs-Eclim](http://fredrik.appelberg.me/2012/02/02/the-ballad-of-emacs-eclim/), \[Fredrik Appelberg\]
* [Emacs and Java](http://blog.senny.ch/blog/2012/10/13/emacs-and-java-journey-of-a-hard-friendship/), \[Yves Senn\]
* [Java Autocompletion for Emacs](http://root42.blogspot.ch/2012/08/java-autocompletion-for-emacs.html), \[root42\]
* [Eclim: Eclipse Meets Vim And Emacs](http://faruk.akgul.org/blog/eclim-eclipse-meets-vim-emacs/), \[Faruk Akgul\]

# Contributing

Have a quick look at our [Contribution Guidelines](doc/CONTRIBUTING.md)
and hack away.

[company-mode]:https://github.com/company-mode/company-mode
[auto-complete-mode]:https://github.com/auto-complete/auto-complete
[melpa]:https://melpa.org/#/
[repo]:https://github.com/emacs-eclim/emacs-eclim

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
