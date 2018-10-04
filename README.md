[![wolf][wolf-img]][wolf]

# [wolf][wolf]

[![Package version][hackage-img]][hackage]
[![Build status][travis-img]][travis]
[![Dependency status][deps-img]][deps]

Wolf is a wrapper around Amazon Simple Workflow Service: it providers a decider
that implements plans, an actor that runs commands, and a registrar that
installs plans. See [examples](examples).

## Getting started
1. Install [Haskell](haskell).
2. Install the Haskell package manager [stack](stack).
3. Setup your global environment:
    1. `mkdir ~/.local/bin` if it doesn't exist already.
    2. add `$HOME/.local/bin` to your `PATH` environment variable.
        - This is usually in `~/.bashrc`, `~/.zshrc`, or similar.
4. Install build dependencies in the `wolf` stack:
    1. `stack install hlint shake shakers happy`
5. Run `stack build`. It should install all dependencies, build binaries, and
   export those binaries to `~/.local/bin`.

## Development

`wolf` has a shakefile/makefile to provide convience around building and testing:

    # build the project's libraries, executables, and tests
    $ ./Shakefile.hs build-tests-error
    
    # test the project
    $ ./Shakefile.hs tests-error
    
    # start an interpreter with the project's libraries, executables, and tests loaded
    $ ./Shakefile.hs ghci-tests
    
    # install the project's executables
    $ ./Shakefile.hs install
    
    # clean the project
    $ ./Shakefile.hs clean
    
    # lint the project source code
    $ ./Shakefile.hs lint
    
    # format the project source code
    $ ./Shakefile.hs format


## Dependencies

To build, install, run, and test `wolf`, the following (global) dependencies may
be required:

+ [stack][stack]
+ [lint][lint]
+ [shake][shake]
+ [shakers][shakers]

## Common build errors

### "Plan construction failed."
When building tests using `./Shakefile.hs build-tests-error`, the ambiguous
`Plan construction failed.` error may rear its head. Make sure your `gcc`
installation is clean (`brew doctor` may help), and make sure you've installed
`stack`, `shake`, and `shakers`.

[haskell]:     https://www.haskell.org/platform/
[wolf]:        https://github.com/swift-nav/wolf
[wolf-img]:    https://cloud.githubusercontent.com/assets/60851/8178609/a077a326-13c4-11e5-9d54-3e417fc6dd6c.jpg
[hackage]:     https://hackage.haskell.org/package/wolf
[hackage-img]: https://img.shields.io/hackage/v/wolf.svg?style=flat
[travis]:      https://travis-ci.org/swift-nav/wolf
[travis-img]:  https://img.shields.io/travis/swift-nav/wolf/master.svg?style=flat
[deps]:        http://packdeps.haskellers.com/feed?needle=wolf
[deps-img]:    https://img.shields.io/hackage-deps/v/wolf.svg?style=flat
[stack]:       https://docs.haskellstack.org/en/stable/README/#how-to-install
[lint]:        https://hackage.haskell.org/package/hlint
[stylish]:     https://hackage.haskell.org/package/stylish-haskell
[shake]:       https://shakebuild.com/
