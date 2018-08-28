# Festral 

This package is developed and adopted for usage with existing extended secos CI system (http://127.0.0.1:81/u.harbuz/secosci) which consists of PHP page, MySQL database and set of scripts for importing
data to the database from the formatted in some way files. This package uses set of configuration files for management of repositories building and testing.

Festral consists of some small utilities:

* `festral build` - utility for building repositories. It takes simple json file with information about what to build, how to build and what branches to build and just do it.
* `festral weles` - utility for communication with Weles tests server.
* `festral test` - utility for performing tests described by yaml files on remote Weles server and recieving results of the tests.
* `festral server` - simple built-in web server for sharing built files and logs.

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
### festral

Since **v.0.6.0** version all subutilities was unified in subcommands in one `festral` command. You can get information about it by typing `festral --help`.
Avaible commands are: `build`, `weles`, `test`.

There is also option `--html-out FILENAME` available without any command which generates HTML summary report about latest builds and tests and put it into file specified by FILENAME.

Also since **v0.6.0** version whole program is configured by file `~/.festral.conf` formatted as below (see example at `Examples/config.json`):

```
{
    "buildLogDir" : "directory where application searches builds",
    "testLogDir" : "directory where application put tests results",
    "welesIP" : "127.0.0.1 - ip address of the Weles server",
    "webPageIP" : "127.0.0.1 - ip of the web page SecosCI located at",
    "welesPort" : "port of the Weles API",
    "welesFilePort" : "Port where output files of the Weles are",
    "reportsDir" : "Directory where reports for web page are located."
}
```
This configuration file was separated from tests descriptions (see `festral test` section).

Other commands are described below:

### festral build

This command clone and build all targets listed in configuration json file: each target is built for every branch listed for this target.

The `festral build` utility has simple command format:

```
 $ festral build (-c <config json>) (-r <repositories location>) (-o <output directory>)
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
 REPO_NAME=#name of the built repository
 BRANCH=#name of the built branch

 #In the tests directories meta.txt has additional fields:
 TESTER=#login of the tester
 TESTER_NAME=#name of the tester
 TEST_TIME=#time where test was performed
 TEST_NAME=#name of the test read from name of the parser script
```

Parser script MUST gets output of the `buildCmd` from its `stdin` after start and writes meta file to the `stdout` IN EXACTLY SUCH ORDER AS fields of metafile
are presented here.

`repository location` is path where directories cloned from `buildRepo`'s of targets projects will be put.

`output directory` is directory where builds results will be put in format 'commithash_buildtime'

After running this command program will do:

  1. clone all repository listed in `config json` to the subdirectory of `repositories location`
  2. checkout for each branch listed in `config json` for current repository and run specified in config build command
  3. pass standard output of build command to the parser specified in the config for this repository
  4. create `meta.txt` file from parsed build output
  5. create directory named as `commitSha1Hash_buildtime` in the `output directory` and put here `meta.txt`, raw build log as `build.log` file
  6. create subdirectory `build_res` and move here all files from parsed `OUT_DIR` meta field
  7. add name of build directory to the `~/.festral/fresh_builds` file and update `~/.festral/build.cache` file by new out files.

--------------
### festral weles

The `festral weles` is utility for communication with remote Weles server.

You can get help about this program by calling:

```
 $ festral weles --help

Usage: festral weles [-a|--all] [-i|--job-id JOB_ID] [-d|--when-done TIME_LIMIT]
                     [-f|--filename FILENAME] [-s|--start-job] [--job-stdout]
                     [--list-files] [-c|--cancel]
  Allow to use Weles API for accessing and managing Weles's jobs by hands.

Available options:
  -a,--all                 List all jobs.
  -i,--job-id JOB_ID       Id of the job to be selected. With no other options
                           list information about this job. (default: -1)
  -d,--when-done TIME_LIMIT
                           Wait until queried job done before doing rest and
                           until TIME_LIMIT is not is now wasted. Job is
                           cancelled after time is out.
  -f,--filename FILENAME   Give filename to the program. What to to with it
                           depends on other selected options.
  -s,--start-job           Start new job passing to the Weles yaml file set by
                           -f option. Returns id of the created job.
  --job-stdout             Print standard output and error streams of job set by
                           -i option.
  --list-files             List all files generated by job set by -i option. If
                           -f option is set print content of that file if it was
                           created by job.
  -c,--cancel              Cancel job specified by -i option.
  -h,--help                Show this help text

```
------------------
### festral test

`festral test` is used for runnig tests on Weles from existing builds and and put it to the directory defined in `~/.festral.conf` configuration file.

It has syntax:

```
Usage: festral test (-r|--run-test TEST_CONFIG_FILE) [-b|--with-build BUILD_DIR]
  Create jobs on remote Weles server with tests defined in .yaml files and
  process responces with results of its execution.

Available options:
  -r,--run-test TEST_CONFIG_FILE
                           Run tests listed in TEST_CONFIG_FILE for specified by
                           -f option build directory. Run for all targets listed
                           in '~/.fresh_builds' file if no target specified.
  -b,--with-build BUILD_DIR
                           Run test only for this build if defined
  -h,--help                Show this help text

```

It is configured by  file passed with -r parameter which is in JSON format with fields as follow:

```
[
        {
            "repo" : "name of the repository",
            "yaml" : "path to the yaml file to use with this repository",
            "name" : "Test name (this is optional field, defoult value is 'undefined')",
            "parser" : "name or path to the binary of the parser of tests output"
        },
        {
            "repo" : "gpapi-tests",
            "yaml" : "/home/tf.yml - example",
            "parser" : "cat"
        }
]
```

Supported built-in test parsers currently are only "TCT" - for tct-test-ta and "XTest" - for xtest made by OPTEE.

You can create own parser scripts for festral-weles. Such script **MUST get log with test result as its FIRST ARGUMENT and put parsed statistics to the stdout**.

Parsed data has format:

```
######################################

Test name,test id, testcase name,result of preparing for test,result of test,result of cleaning after test,spent time

...

######################################
```

Result of test can be TEST_PASS or TEST_FAIL.

Example of such output:

```
###########################################################
Xtest,0,regression_1001,TEST_PASS,TEST_PASS,TEST_PASS,0.0
Xtest,1,regression_1002,TEST_PASS,TEST_PASS,TEST_PASS,0.0
Xtest,2,regression_1003,TEST_FAIL,TEST_FAIL,TEST_FAIL,0.0
Xtest,3,regression_1004.1,TEST_FAIL,TEST_FAIL,TEST_FAIL,0.0
Xtest,4,regression_1004.2,TEST_FAIL,TEST_FAIL,TEST_FAIL,0.0
Xtest,5,regression_1004.3,TEST_FAIL,TEST_FAIL,TEST_FAIL,0.0
Xtest,21,regression_1010.1,TEST_PASS,TEST_PASS,TEST_PASS,0.0
Xtest,22,regression_1010.2,TEST_PASS,TEST_PASS,TEST_PASS,0.0
Xtest,23,regression_1010.3,TEST_PASS,TEST_PASS,TEST_PASS,0.0
Xtest,24,regression_1010.4,TEST_PASS,TEST_PASS,TEST_PASS,0.0
Xtest,25,regression_1010.5,TEST_PASS,TEST_PASS,TEST_PASS,0.0
###########################################################
```

Example of the bash script for parsing XTest is at `Examples/own_xtest_parser.sh`

When you run `festral test` command, it will do actions:

  1. read every record in `TEST_CONFIG_FILE` and go to the properly build directory
  2. read YAML template specified in config for this test and make really YAML file from it: find existing packages listed in `~/.festral/build.cache`
  and replace templated names with it, get field `webPageIP` form `~/.festral.conf` file and create uri for downloading packages from this IP using format:
  
  http://`webPageIP`/secosci/download.php?file=`resolved_package_name`&build=`resolved_build_dir_name`/build_res
  
  so there is `download.php` script must exists under `webPageIP` adress and it must accept parameters `file` and `build`.
  
  3. send this yaml for the Weles server and wait for an 1 hour for test finishes, if not, cancel this mjob and run next test
  4. if test finished, pass output of test to the specified in config parser. 
  
  **If it is built-in parser,** only `tct-test-ta.log` is passed for **TCT** parser
  and only `xtest.log` is passed for **XTest** parser, So **YOU MUST REDIRECT THESE TEST OUTPUTS TO THE RIGHT FILES IN YOUR YAML FILE**.
  
  If you yse your own parser, all output files from server will be passed to the test results parser
  
  5. create directory named as `buildCommitSha1Hash_testTime` in the directory specified in `testLogDir` field of the `~/.festral.conf` file
  and put here files: `build.log` - meta file from tested build; `meta.txt` - extended meta file with additional informations about testing
  `report.txt` - parsed test results; `tf.log` - whole output of the test process
  
  6. put names of new test logs to the `~/.festral/fresh_tests`

----------------
### festral server
This commant is used for setting up simple web server which can be used for sharing built files with remote Weles server of just show logs and reports in the web browser.

The syntax of this command is simple:
```
festral server -p PORT_NUMBER
```
where PORT_NUMBER is just number of port where server will listen to.

Web pages of this server have API as follow:

  * `/secosci/reports[.php]` - page with listed reports files
  * `/secosci/reports[.php]?file=filename` - show specified by filename report
  * `/secosci/getlog[.php]?type=type&hash=hash&time=time` - show log specified by type: it can be `build` for build log and `test` for test log.Log also must be specified by hash and time of the build/test.
  * `/secosci/download[.php]?file=filename&build=build_dir` - link for downloading of the file with `filename` from the build directory specified by `build_dir` parameter

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
* `##TEMPLATE_RPM_INSTALL_CURRENT packagename##` - install package specified by name on target using `rpm`. It is more generic replacement for 

```
- push:
    ##TEMPLATE_URL packagename##
    dest: '/tmp/packagename.rpm'
    alias: 'packagename.rpm'
- run:
    name: "'rpm -i /tmp/packagename.rpm --force 2>&1 >> /tmp/install.log'"
```

* `##TEMPLATE_RPM_INSTALL_LATEST packagename##` - same as `##TEMPLATE_RPM_INSTALL_CURRENT packagename##` but use `##TEMPLATE_LATEST name##` instead of `##TEMPLATE_URL name##`.

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


