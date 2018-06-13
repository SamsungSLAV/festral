# Festral 

This package is developed and adopted for usage with existing extended secos CI system (http://127.0.0.1:81/u.harbuz/secosci) which consists of PHP page, MySQL database and set of scripts for importing
data to the database from the formatted in some way files. This package uses set of configuration files for management of repositories building and testing.

Festral consists of some small utilities:

* `festral-build` - utility for building repositories. It takes simple json file with information about what to build, how to build and what branches to build and just do it.
* `festral-weles` - utility for communication with Weles tests server. It can send testcases described by yaml file and recieve results of the tests.

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
-----------
### festral-build

This command clone and build all targets listed in configuration json file: each target is built for every branch listed for this target.

The `festral-build` utility has simple command format:

```
 $ festral-build <config json> <repositories location> <output directory>
```

The main part of these arguments is `config json` which contains description of build targets. 
It has format described below:

```
 [
    {
        # Name of the repository, remote repository will be cloned to the directory with this name under <repositories location>
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

`meta.txt` file has format:

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

`output directory` is directory where builds results will be put in format 'commithash_buildtime'

--------------
### festral-weles

The `festral-weles` is utility for communication with remote Weles server, for management of testing process and for getting tests results.

You can get help about this program by calling:

```
 $ festral-weles --help
Festral - simple client for tests management using Weles as test server

Usage: festral-weles [-a|--all] [-i|--job-id JOB_ID] [-d|--when-done]
                     [-f|--filename FILENAME] [-s|--start-job] [--job-stdout]
                     [--list-files] [-r|--run-test]
    Create jobs on remote Weles server with tests defined in .yaml files and
    process responces with results of its execution.

Available options:
    -a,--all                 List all jobs.
    -i,--job-id JOB_ID       Id of the job to be selected. With no other options
                             list information about this job. (default: -1)
    -d,--when-done           Wait until queried job done before doing rest.
    -f,--filename FILENAME   Give filename to the program. What to to with it
                             depends on other selected options.
    -s,--start-job           Start new job passing to the Weles yaml file set by
                             -f option. Returns id of the created job.
    --job-stdout             Print standard output and error streams of job set by
                             -i option.
    --list-files             List all files generated by job set by -i option. If
                             -f option is set print content of that file if it was
                             created by job.
    -r,--run-test            Run test for specified by -f option build directory.
                             Run for all targets listed in '~/.fresh_builds' file
                             if no target specified.
    -h,--help                Show this help text


```

`festral-weles` uses configuration JSON file located at `~/.festral.conf` formatted as below:

```
{
    "buildLogDir" : "directory where application searches builds",
    "testLogDir" : "directory where application put tests results",
    "welesIP" : "127.0.0.1 - ip address of the Weles server",
    "welesPort" : "port of the Weles API",
    "welesFilePort" : "Port where output files of the Weles are",
    "yamls" : [
        {
            "repo" : "name of the repository",
            "yaml" : "path to the yaml file to use with this repository",
            "parser" : "name or path to the binary of the parser of tests output"
        },
        {
            "repo" : "gpapi-tests",
            "yaml" : "/home/tf.yml - example",
            "parser" : "cat"
        }
    ]
}

```

Supported built-in test parser is currently only "TCT" - for tct-test-ta
