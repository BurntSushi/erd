0.2.1.0 (2020-02-01)

* Add -v command-line flag to show versions and git revision.

* Support of regular dot-tables to visualize entities. (PR #74)

* Text alignment in a Cell of a Table is explicitly defined. (PR #64)

* New build target using _static-haskell-nix_ to create statically linked erd. (PR #61)

* Updating code to be compliant with ghc-8.6.

* Resolving compiler warnings that were muted by earlier setup.

* Adding optional '-c/--config' switch to use _~/.erd.yaml_ as configuration file (PR #68)

0.2.0.0 (2019-03-26)

* Adding test suite. (PR #46)

* Adding TravisCI script. (PR #46)

* Not falling to *bottom* when GraphViz does not exists on the host system. (PR #45)

* Supporting to build with *stack* and/or *cabal*. (PR #43, PR #18)

* Adding optional -e command-line argument to determine the type of edges
  between nodes/tables. (PR #13)

* Allow spaces in table identifiers, quoted with backticks. (PR #9)

0.1.3.0
