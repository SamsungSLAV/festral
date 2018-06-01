# Festral 
Package with tools for tests management using Weles as test server.

Festral consists of some small utilities:

..* `festral-build` - utility for building repositories. It takes simple json file with information about what to build, how to build and what branches to build and just do it.
..* `festral-weles` - utility for communication with Weles tests server. It can send testcases described by yaml file and recieve results of the tests.

----------
### How to build
Installation scripts presented below are made on Ubuntu 16.04.

`Festral` is written in Haskell and for building it from sources your need to have installed `haskell-platform` package for your distribution:

```
 $ sudo apt install haskell-platform
```

The next step is to install `cabal` package manager which will configure `Festral's` sources, resolve its dependencies, build and install package for you:

``` 
 $ sudo apt install cabal-install
 $ cabal update
 ```

Now you can build the package. Go to the root directory of the `Festral` sources (it contains Setup.hs file) and run:

```
 $ sudo apt install libghc-curl-dev
 $ cabal install --only-dependencies
 $ runhaskell Setup.hs configure --user
 $ runhaskell Setup.hs build
 $ runhaskell Setup.hs install
```

If build finished with success, you will find festral executable in the directory `dist/build/festral/festral-build/festral-build` and `dist/build/festral/festral-weles/festral-weles`

-----------
# Usage

### festral-build

The `festral-build` utility has simple command format:

```
 $ festral-build <config json> <repositories location> <output directory>
```

The main part of these arguments is `config json` which contains description of build targets. 
It has format described below:

```
 [
    {
        # Name of the repository, it MUST be name of the directory containing repository to build located in the <repositories location>
        "buildName" : "tct-test-ta", 
        # Command to be used for build this repository
        "buildCmd" : "gbs build -A armv7l -P tizen_vd", 
        # remote address of the repository to build. 
        "buildRepo" : "git@127.0.0.1:l.kostyra/tct-test-ta.git", 
        # Name of the parser of the build results. See description below.
        "buildResParser" : "GBS", 
        # List of the branches to be built. Every one from these will be built for the target.
        "branches" : ["master", "arm", "devel"] 
    },
    { ... Some other build targets ... }
 ]
```

Parser is some script or binary which generates meta.txt file from output of your `buildCmd` command.

meta.txt file has format:

```
 BOARD=#name of the board or arch of target
 BUILD_TYPE=#debug or somthing else, I don't know for what it is
 COMMIT=#name of the built commit
 BUILD_TIME=#build time in format YYYYMMDDHHMMSS
 TOOLCHAIN=#name of toolchain used for build
 BUILDER=#username of builder
 BUILD_STATUS=#result of build (SUCCEED and FAILED are known, but may be there are other ones)
 BUILD_HASH=#hash of the build
```

Parser script MUST gets output of the `buildCmd` from its `stdin` after start and writes meta file to the `stdout`.

`repository location` is path where directories where cloned from `buildRepo`'s of targets projects will be put.

`output directory` is directory where builds results will be put.
