There really isn't much here. Carry on!

# Setup

* Install and set the expected tool versions using ghcup.
  ```
  ghcup install ghc --set 9.8.1
  ghcup install cabal --set 3.10.2.1
  ```

* (Optionally) install the local git pre-commit hook
  ```
  cp localhooks/pre-commit .git/hooks
  ```
  This hook only validates that the freeze files being checked in
  appear to be in sync.

# Building the Blog

```
cabal build blog build
```
