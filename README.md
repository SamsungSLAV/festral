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
 $ festral-build (-c <config json>) (-r <repositories location>) (-o <output directory>)
```

The main part of these arguments is `config json` which contains description of build targets. 
It has format described below (see example of this file in `Examples/buildconfig.json`):

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

Parser is some script or binary which generates meta.txt file from output of your `buildCmd` command. Now only "GBS" parser is stable.

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
 REPO_NAME=#name of the built repository
 BRANCH=#name of the built branch

 #In the tests directories meta.txt has additional fields:
 TESTER=#login of the tester
 TESTER_NAME=#name of the tester
 TEST_TIME=#time where test was performed
```

Parser script MUST gets output of the `buildCmd` from its `stdin` after start and writes meta file to the `stdout` IN EXACTLY SUCH ORDER AS fields of metafile
are presented here.

`repository location` is path where directories cloned from `buildRepo`'s of targets projects will be put.

`output directory` is directory where builds results will be put in format 'commithash_buildtime'

The command:
```
 $ festral-build --html-out FILENAME 
```
generates HTML report about the latest builds and performed tests and save it with given filename.

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

`festral-weles` uses configuration JSON file located at `~/.festral.conf` formatted as below (see example at `Examples/config.json`):

```
{
    "buildLogDir" : "directory where application searches builds",
    "testLogDir" : "directory where application put tests results",
    "welesIP" : "127.0.0.1 - ip address of the Weles server",
    "webPageIP" : "127.0.0.1 - ip of the web page SecosCI located at",
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

Supported built-in test parsers currently are only "TCT" - for tct-test-ta and "XTest" - for xtest made by OPTEE.

-----------------
### Test cases description

Tests are described by YAML files used by `Weles` but extended with templates syntax (see examles at `Examples/*.yml`)

You can use templated rows in your yamls according below syntax:

* temlate fragment starts and finishes with `##` symbols.
* `##TEMPLATE_URL filename##` - replace given filename with `uri` for the file with specified name (or if specified filename is part of the real filename)
    from the current build only. If no such file made by the current build this link can be invalid.
    
    Example: `##TEMPLATE_URL tef-libteec##` can be replaced by row
    `uri: 'http://127.0.0.1/secosci/download.php?file=tef-libteec-0.0.1-0.armv7l.rpm&build=c2ac26bd548e04ddd5ef5150f600172048f2fcfa_20180622210245/build_res'`
    and `Weles` will can download this package by generated link.
* `##TEMPLATE_LATEST packagename##` - replace given filename with uri to the latest built version of the specified package if it ever been built by the `Festral`.
    
    Example: `##TEMPLATE_LATEST tf##` can be replaced with `uri: 'http://127.0.0.1/secosci/download.php?file=tf-0.0.1-0.armv7l.rpm&build=c2ac26bd548e04ddd5ef5150f600172048f2fcfa_20180622210245/build_res'`.
    You can push packages from other repositories built by `festral-build` to the `Weles` using this template.

------------------

### How it works

This package is developed for using with [Weles API for device farm](https://git.tizen.org/cgit/tools/weles/) and modified [SecosCI system](http://127.0.0.1:81/u.harbuz/festral/tree/master).
It uses API of `SecosCI` for generated output (except HTML reports) and remote device farm for performing tests.


![Graphics scheme of the CI system modules](Docs/general_scheme.png "CI modules").


The typical usage example for automated running tests with `cron`:

```

0 21 * * * ./bin/festral-build -c /home/secosci/bin/festral-build.config.json -r /home/secosci/secos-repo/ -o /home/secosci/build-log/
0 0 * * * ./bin/festral-weles -r
0 1 * * * ./bin/festral-build --html-out /home/secosci/www/reports/$(date +\%Y\%m\%d\%H\%M).html
0 1 * * * ./secos-repo/secosci/sql/import_build.sh
0 1 * * * ./secos-repo/secosci/sql/import_test.sh
```

Steps which are executed:

1. building repositories listed in `buildconfig.json` by `festral-build`
2. running tests for new builds on `Weles` by `festral-weles`
3. generate summary HTML report
4. import new builds to `secosci` database by `import_build.sh` script
5. import new tests results to the `secosci` database by `import_test.sh` script
6. now new builds and results are visible on the [secosci](http://127.0.0.1/secosci/) and new daily report is avaible [at this page](http://127.0.0.1/secosci/reports.php)


