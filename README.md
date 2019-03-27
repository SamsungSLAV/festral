# Table of Contents
- [Festral](#festral)
   * [How to build](#how-to-build)
        + [Host build](#local-build)
        + [Docker build](#docker-build)
   * [Generating documentation](#source-code-documentation)
- [Usage](#usage)
   * [Configuration](#configuration)
   * [Automated building software](#festral-build)
   * [Automated testing software](#festral-test)
   * [Make reports](#festral-report)
        + [Text report](#text-report)
        + [HTML report](#html-report)
- [Other utilities](#festral-weles)
   * [Weles client](#festral-weles)
   * [Boruta client](#farmer)
   * [File serwer](#festral-server)
- [Test case description](#test-cases-description)
   * [YAML templates](#yaml-templates)
   * [File preprocessor](#file-preprocessor)
- [How it works](#how-it-works)

# Festral

This package provides client for SLAV testing framework and more abstract layer
for performing building and testing repositories by user.

Festral consists of some small utilities:

* `festral build` - utility for building repositories. It takes simple json file
with information about what to build, how to build and what branches to build
and just do it.
* `festral test` - utility for performing tests described by yaml files on
remote Weles server and recieving results of the tests.
* `festral report` - generate reports in various formats
* `festral weles` - utility for communication with Weles tests server.
* `festral server` - simple built-in web server for sharing built files and
logs.

It additionaly has utility named `farmer` for accessing devices of Boruta farm.

----------
### How to build

#### Local build

Installation scripts presented below are made on Ubuntu 16.04.

`Festral` is written in Haskell and for building it from sources your need to
have installed `haskell-platform` and `curl` developement
package for your distribution:

```
 $ sudo apt install haskell-platform libghc-curl-dev
```

Now you can build the package. Go to the root directory of the `Festral`
sources (it contains Setup.hs file) and run `make` for build or
`make package` for make debian package which will be located in deb
subdirectory.

If build finished with success, you will find festral executable in the
`dist/build/` directory.

#### Docker build

There is also Docker build available: just run

```
 $ make docker
```

This command will build `festral` and `farmer` binaries and Debian package
inside the Docker container from local sources. The output will be placed
at `bin` directory in the source root.

For make code documentation using docker, run
```
 $ make docker-docs
```
Documentation will be generated at `dist/doc` directory.

-----------
### Source code documentation
Source code of Festral is documented using `haddock`. For generate documentation
just run:

```
 $ make docs
```

Documentation will be generated under `dist/doc/` directory.

# Usage
-----------
### festral

You can get information about it by typing `festral --help`.
Available commands are: `build`, `test`, `report`, `weles`, `server`.

### configuration

Whole program is configured by file
`~/.festral.conf` formatted as below (see example at `Examples/config.json`):

```
{
    "buildLogDir" : "/diretory/for/builds",
    "testLogDir" : "/directory/for/tests",
    "welesIP" : "127.0.0.1",
    "welesPort" : 5010,
    "welesFilePort" : 8888,
    "festralIP" : "your machine IP",
    "festralPort" : 8888,
    "serverRoot" : "/root/directory/for/your/fileserver",
    "borutaIP" : "boruta IP",
    "borutaPort" : 6666
}
```
Subcommands are described below:

### festral build

This command clone and build all targets listed in configuration json file:
each target is built for every branch listed for this target.

This command writes names of the builds to the `stdout` or into the file if `-o`
option is specified.

The `festral build` utility has simple command format:

```
 $ festral build (-c <config json>) (-r <repositories location>) [-o <output directory>]
```

The main part of these arguments is `config json` which contains description
of build targets.
It has format described below (see example of this file in
`Examples/buildconfig.json`):

```
 [
    {
        # Name of the repository, remote repository will be cloned to the
        #directory with this name under <repositories location>
        "buildName" : "test",
        # Command to be used for build this repository
        "buildCmd" : "gbs build -A armv7l",
        # remote address of the repository to build.
        "buildRepo" : "git@127.0.0.1/test.git",
        # Name of the parser of the build results. See description below.
        "buildResParser" : "GBS",
        # List of the branches to be built. Every one from these will be built
        #for the target.
        "branches" : ["master", "arm", "devel"]
    },
    { ... Some other build targets ... }
 ]
```

Parser is some script or binary which generates meta.txt file from output
of your `buildCmd` command.

`meta.txt` file has format:

```
 BOARD=#name of the board or arch of target
 BUILD_TYPE=#debug or somthing else, I don't know for what it is
 COMMIT=#name of the built commit
 BUILD_TIME=#build time in format YYYYMMDDHHMMSS
 TOOLCHAIN=#name of toolchain used for build
 BUILDER=#username of builder
 BUILD_STATUS=#result of build (SUCCEED and FAILED are known, but may be
 #there are other ones)
 BUILD_HASH=#hash of the build
 REPO_NAME=#name of the built repository
 BRANCH=#name of the built branch
 OUT_DIR=#output directory where build put built targets

 #In the tests directories meta.txt has additional fields:
 TESTER=#login of the tester
 TESTER_NAME=#name of the tester
 TEST_TIME=#time where test was performed
 TEST_NAME=#name of the test
 TEST_STATUS=#Status of the executed tests. See below for possible values.
 TEST_DEVICE=#Name of the device which test was performed on.
```

Parser script MUST gets output of the `buildCmd` from its `stdin` after start
and writes meta file to the `stdout`.

`repository location` is path where directories cloned from `buildRepo`'s of
targets projects will be put.

`output directory` is directory where builds results will be put in format
`commithash_buildtime`

After running this command program will do:

  1. clone all repository listed in `config json` to the subdirectory of
  `repositories location`
  2. checkout for each branch listed in `config json` for current repository
  and run specified in config build command
  3. pass standard output of build command to the parser specified in the
  config for this repository
  4. create `meta.txt` file from parsed build output
  5. create directory named as `commitSha1Hash_buildtime` in the
  `output directory` and put here `meta.txt`, raw build log as `build.log` file
  6. create subdirectory `build_res` and move here all files from parsed
  `OUT_DIR` meta field
  7. add name of build directory to the `~/.festral/fresh_builds` file and
  update `~/.festral/build.cache` file by new out files.

Currently possible statuses for `TEST_STATUS`:
  - `BUILD FAILED` - test was not started because repository under test has
  not built successfully.
  - `YAML NOT FOUND` - YAML file from test config for this repository does
  not exist
  - `NO JOB STARTED` - Weles job has not been started because some error occured
  - `DEVICE FAILED` - Device Under Test (DUT) was failed what means that
  communication between Mux-Pi and DUT was lost or command executed on DUT exit
  with fatal error or some other problems appeared on tested board.
  - `DOWNLOAD FILES ERROR` - failed to download all artifacts by Weles server:
  it usually means that one of the URI appeared in YAML file is invalid or that
  there is no free space on the Weles server.
  - `WELES ERROR` - test job failed with some other error: error message is in
  log file
  - `SEGFAULT` - `segmentation fault` error was appeared during testing process,
  so pass/all test rating can be not complete
  - `COMPLETE` - test executed on Dryad with no errors. **It doesn't mean that
  tests passed successfully**, for see test results see test output generated by
  test parser (see `festral test`).

------------------
### festral test

`festral test` is used for runnig tests on Weles from existing builds and and
put it to the directory defined in `~/.festral.conf` configuration file.

It has syntax:
```
festral test (-r|--run-test TEST_CONFIG_FILE) [-o|--out FILENAME]
             [BUILD_DIRS...]
```
where `TEST_CONFIG_FILE` is JSON configuration file described below,
`BUILD_DIRS` are names of builds returned by `festral build` passed as
arguments. If there is no `BUILD_DIRS` passed test will be performed for
latest builds listed in the `~/.festral/fresh_builds` file.

This command returns names of the performed tests to the `stdout` or into the
file specified by `-o` option.

It is configured by file passed with -r parameter which is in JSON format with
fields as follow:

```
[
        {
            "repo" : "name of the repository",
            "yaml" : "path to the yaml file to use with this repository",
            "name" : "Test name (this is optional field, defoult value is 'undefined')",
            "parser" : "name or path to the binary of the parser of tests output",
            "runTTL" : 3600 - time to live of the test job after start of execution,
            "timeout": 18000 - time to live of test job from it was created even it was waiting only,
            "targets" : ["rpi3", "PC"] - list of targetw to run this test on
        },
        {
            "repo" : "tests",
            "yaml" : "/home/tests.yml",
            "parser" : "~/test_parser.sh",
            "targets" : ["rpi3"]
        }
]
```

Supported built-in test parsers currently are only "Default" - for simple `OK`
by line for every passed test, and other line is treated as failed test;
and "XTest" - for xtest made by OPTEE.

You can create own parser scripts for festral-weles. Such script
**MUST get log with test result as its** `stdin` **and put parsed
statistics to the stdout**.

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

Example of the bash script for parsing XTest is at
`Examples/own_xtest_parser.sh`

When you run `festral test` command, it will do actions:

  * read every record in `TEST_CONFIG_FILE` and go to the properly build
  directory
  * read YAML template specified in config for this test and make really YAML
  file from it: find existing packages listed in `~/.festral/build.cache`
  and replace templated names with it, get field `webPageIP` form
  `~/.festral.conf` file and create uri for downloading packages from
  this IP using format:

  http://`festralIP`:`festralPort`/download?file=`resolved_package_name`&build=`resolved_build_dir_name`/build_res

  * send this yaml for the Weles server and wait for an 1 hour for test
  finishes, if not, cancel this mjob and run next test
  * if test finished, pass output of test to the specified in config parser.

  **Only** `test.log` **file is passed to the test parser**
  so **YOU MUST REDIRECT THESE TEST OUTPUTS TO THE RIGHT FILE
  IN YOUR YAML FILE**.

  * create directory named as `buildCommitSha1Hash_testTime` in the directory
  specified in `testLogDir` field of the `~/.festral.conf` file
  and put here files: `build.log` - meta file from tested build;
  `meta.txt` - extended meta file with additional informations about testing
  `report.txt` - parsed test results; `tf.log` - whole output of the
  test process

  * put names of new test logs to the `~/.festral/fresh_tests`
----------------
### festral report

There are HTML and formatted by user text reports are available.

Report command expects names of the builds and tests as arguments, but if no
arguments are given, it create report for latest performed builds and tests.
You can specify output file for the report by -o option.

#### Text report

For generate simple text report use command:
```
 $ festral report --text-report f457004fdc7326c8125936b5e600c0d330c89404_20180918161950 f457004fdc7326c8125936b5e600c0d330c89404_20180918161949 f457004fdc7326c8125936b5e600c0d330c89404_20180918161948
```
where hashes are names of the tests returned by festral test command or
builds returned by festral build command.

There is option `-f` for text report which can set format string for the report.
It supports specifiers like below:


Make text report when every line has a format like passed in first argument.
Format string has special characters:

|Specifier|    Output             |    Example                              |
|---------|-----------------------|-----------------------------------------|
| %b      | board                 | armv7l                                  |
| %t      | build type            | debug                                   |
| %c      | commit name           | 60fbeee6f89e2a61417033a854b3d2fdfc9f1a58|
| %T      | build time            | 20181009112502                          |
| %C      | toolchain             | GBS                                     |
| %u      | builder username      | test.user                               |
| %s      | build status          | SUCCEED                                 |
| %h      | build hash            | 60fbeee6f89e2a61417033a854b3d2fdfc9f1a58|
| %o      | build output directory| /GBS-ROOT/local/tizen_arm/armv7l/RPMS   |
| %r      | name of the repository| some-test                               |
| %B      | branch name           | master                                  |
| %l      | tester login          | tester.login                            |
| %L      | tester name           | Kowalski                                |
| %e      | test time             | 20181009112502                          |
| %n      | test name             | SOME TEST                               |
| %S      | test status           | COMPLETE                                |
| %d      | test device           | rpi3                                    |
| %R      | pass rating passed/all| 55/210                                  |
|%%       | insert % character    | %                                       |

Default format is \"%r[%B] Build: %s Test: %R\".

Some examples:

```
$ festral report --text-report
-------------------- Result outputs -----------------------
my_prog[optee] Build: FAILED Test: BUILD FAILED
my_prog[arm] Build: SUCCEED Test: DOWNLOAD FILES ERROR
my_prog[master] Build: SUCCEED Test: DOWNLOAD FILES ERROR
my_prog[optee] Build: FAILED Test: BUILD FAILED
```
```
$ festral report --text-report -f "Repo %r built with %s status"
-------------------- Result outputs -----------------------
Repo my_prog built with FAILED status
Repo my_prog built with SUCCEED status
Repo my_prog built with SUCCEED status
Repo my_prog built with FAILED status
```
```
$ festral report --text-report -f "commit %c on branch %B"
-------------------- Result outputs -----------------------
commit db8f9abd26c0be5cc171c319d0072c880cb95abe on branch optee
commit 75c2a4d605c16e5d4375e4d672df8e22cb6ded41 on branch arm
commit 3fa921584376c5ca64f282c833597bff5b72c8cc on branch master
commit db8f9abd26c0be5cc171c319d0072c880cb95abe on branch optee
```
#### HTML report

You can also generate HTML reports using own template page with your JS and
other feathers. For put in your file HTML table with build results insert
`##TEMPLATE_BUILD_TABLE id##` into your HTML code, and for put test results put
`##TEMPLATE_TEST_TABLE id##`, where id is HTML element’s id of this table.

Then run command:
```
 $ festral report --html-report -f template.html f457004fdc7326c8125936b5e600c0d330c89404_20180918161950  f457004fdc7326c8125936b5e600c0d330c89404_20180918161949 f457004fdc7326c8125936b5e600c0d330c89404_20180918161948
```
It will fill your html template by tables with data. If there is no template
passed it will use default simple HTML template.

--------------
### festral weles

The `festral weles` is low-level utility for communication with remote Weles
server.

You can get help about this program by calling:

```
 $ festral weles --help

```

For starting single test use
```
 $ festral weles -s yaml_file.yml
```
This command writes id of the new job to the console, which you can pass to
the -i option and get information about job. For example:
```
 $ festral weles -i 112233
```
will show status of job with id 112233,
```
 $ festral weles -i 112233 --job-stdout
```
will print standard output of the test job
```
 $ festral weles -i 112233 --list-files -f test12348.txt
```
will print content of the file owned by Weles with name test12348.txt if it is
in the artefactorium of this job.

To see list of files use
```
 $ festral weles -i 112233 --list-files
```
------------------
### farmer

The `farmer` is tool for connecting to devices registered in the
Boruta's farm of SLAV stack. This tool is more low-lewel then even
`festral weles`, but it could
be helpful when you need to have console for device under test and its MuxPi.

You can get list of all workers (devices) by calling
```
 $ farmer -w
```
Now devices are identified by its `device_type` field of the output JSON,
so if you are want to have console e.g. for some `rpi3` board, you can
get it by command
```
 $ farmer --console-device rpi3
```
This command will open ssh session which will be **valid not more than 1 hour**
for one of the free boards of that type. If all devices of this type are busy,
this command will not allow to connect existing session
without `--force` option (force connection to the opened session may broke
other's work, use it only if you are know you are doing).

This method is bad because you couldn't use this device by UUID during
this session.

list all requests and statuses of Boruta:
```
 $ farmer -a
```
Open ssh console for MuxPi specified by its UUID (you can find it in list of
workers, see `farmer -w`):
```
 $ farmer --console-uuid 355e0604-7832-4c21-948c-86c55989118f
```
Push file `~/file_from` from host to the MuxPi as /tmp/test.json with UUID:
```
 $ farmer push -u 355e0604-7832-4c21-948c-86c55989118f ~/file_from -o /tmp/test.json
```
Push file `~/file_from` from host to the device under test directly
(e.g. for Raspberry Pi3 connected to this MuxPi) as /tmp/test.json with UUID:
```
 $ farmer push -u 355e0604-7832-4c21-948c-86c55989118f ~/file_from -o /tmp/test.json --dut
```
Boot device up:
```
 $ farmer --boot 355e0604-7832-4c21-948c-86c55989118f
```
Execute command `uname -a` on device under test:
```
 $ farmer exec -u 355e0604-7832-4c21-948c-86c55989118f "uname -a" --dut
```
----------------
### festral server
This command is used for setting up simple web server which can be used for
sharing files with remote Weles server of just show logs and reports in
the web browser.

The syntax of this command is simple:
```
festral server -p PORT_NUMBER
```
where PORT_NUMBER is just number of port where server will listen to.

Web pages of this server have API as follow:

  * `/files` - page with listed files in root of the server
  * `/files/path/to/filename` - show specified by filename file (download it if it's not `html` or `css`)
  * `/getlog[.php]?type=type&hash=hash&time=time` - show log specified
  by type: it can be `build` for build log and `test` for test log.Log also
  must be specified by hash and time of the build/test.
  * `/download[.php]?file=filename&build=build_dir` - link for
  downloading of the file with `filename` from the build directory specified
  by `build_dir` parameter

-----------------
### Test cases description

Tests are described by YAML files used by `Weles` but extended with
templates syntax (see examles at `Examples/*.yml`) and preprocessor with logic
defind by user.

#### YAML templates

You can use templated rows in your yamls according below syntax:

* temlate fragment starts and finishes with `##` symbols.
* `##TEMPLATE_URL filename##` - replace given filename with `uri` for
the file with specified name (or if specified filename is part of the
real filename)
    from the current build only. If no such file made by the current build
    this link can be invalid.

    Example: `##TEMPLATE_URL tef-libteec##` can be replaced by row
    `uri: 'http://127.0.0.1/download?file=tef-libteec-0.0.1-0.armv7l.rpm&build=c2ac26bd548e04ddd5ef5150f600172048f2fcfa_20180622210245/build_res'`
    and `Weles` will can download this package by generated link.
* `##TEMPLATE_LATEST packagename##` - replace given filename with uri to the
latest built version of the specified package if it has ever been built by
the `Festral`.

    Example: `##TEMPLATE_LATEST tf##` can be replaced with `uri: 'http://127.0.0.1/download?file=tf-0.0.1-0.armv7l.rpm&build=c2ac26bd548e04ddd5ef5150f600172048f2fcfa_20180622210245/build_res'`.
    You can push packages from other repositories built by `festral build`
    to the `Weles` using this template.
* `##TEMPLATE_RPM_INSTALL_CURRENT packagename##` - install package specified
by name on target using `rpm`. It is more generic replacement for

```
- push:
    ##TEMPLATE_URL packagename##
    dest: '/tmp/packagename.rpm'
    alias: 'packagename.rpm'
- run:
    name: "'rpm -i /tmp/packagename.rpm --force 2>&1 >> /tmp/install.log'"
```

* `##TEMPLATE_RPM_INSTALL_LATEST packagename##` - same as
`##TEMPLATE_RPM_INSTALL_CURRENT packagename##` but use
`##TEMPLATE_LATEST name##` instead of `##TEMPLATE_URL name##`.
* `##TEMPLATE_FILE filename##` - insert into this place content of the file
specified by `filename`. It allows separate similar parts of the yamls by
files for ommiting repeating of code.
* `TEMPLATE_RUN_TEST cmd` - run test command and put its output into the
test.log file.
* `TEMPLATE_RUN cmd` - run command on target device.

#### File preprocessor

Since `Festral 2.0` new language of test case is introduced which is described below.

File which is written in Festral test case preprocessor language **must have** `.ftc` extension.
Otherwise it is interpreted as raw `YAML` file template.

##### Syntax

The abstract syntax tree of the preprocessor language is presented below:

```

 b    ::= true | false | not(b) | b opb b | w cmp w
 opb  ::= && | ||

 w    ::= %text | "text" | @text | [w.]

 cmp  ::= == | !=

 stmt ::= raw (text) | if (b) stmt else stmt | include (w)
          | insert (w) | [stmt;]

 comments: /* */
```

`b` is the boolean expressionwhixh can has values `true` or `false` or negated
boolean expression or binary operation with two boolean expressions or result of
comparsion of two words expression.

`opb` - the binary boolean operation, just **AND** and **OR**.

`cmp` - comparsion of two words expression (equals or not equals) that returns boolean value.

`w` - word is one of three possible lexemes:

- `%name` - insert in this place value of test configuration field with given name (see below)
- `"text"` - just string literal
- `@name` - insert in this place value of operating system environment variable specified by name

Words can be concatenated by `.` symbol, e.g.
```
include(@HOME."/myfile.ftc")
```

`stmt` - statement which can be one of below:

- `raw(text)` - insert in this place text given in parenses.
- `if (b) stmt else stmt` - if boolean expression is evaluated as `true` insert
the first statement, else insert the second statement.
- `include(name)` - insert in this place preprocessed content of given file if this file is `.ftc`,
otherwise insert just raw content of that file.
- `insert(w)` - insert into this place value of evaluated word (test config value,
environment variable or string literal)

Statements are separated by `;` (semicolon) symbol. The last statement **have not** to have semicolon after it.

There are available test configuration fields, which will be expanded to the
real value by preprocessor:

`repo`, `parser`, `name`, `target`, `yaml`.

------------------

### How it works

The typical usage example for automated running tests with `cron`:

```
0 21 * * * ./bin/festral build -b /home/bin/festral-build.config.json -r /home/secos-repo/
0 0 * * * ./bin/festral test -r /home/bin/festral-test.config.json
0 2 * * * ./bin/festral --html-report -o /home/www/reports/$(date +\%Y\%m\%d\%H\%M).html -f /home/www/template.html
```

Steps which are executed:

1. building repositories listed in `buildconfig.json` by `festral build`
2. running tests for new builds on `Weles` by `festral weles`
3. generate summary HTML report
