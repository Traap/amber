### Automated testing
**amber** is a Ruby gem that is used to coordinate automated testing.  **amber**
was designed to automation various types of testing that require creating and
assembling detailed records intended to prove a product or tool has been tested
for its intended purpose.  **amber** borrows from a Ruby-on-Rails concept of
convention over configuration.  In this regard, amber consumes YAML documents
that are placed beneath a factory directory root.  You are encouraged to review
**amber's** report directory because it has been designed to demonstrate
**amber's** capabilities.
[example.pdf](https://github.com/Traap/amber/blob/master/doc/example.pdf) has
been provided to demonstrate the automation framework components amber,
[autodoc](https://github.com/Traap/autodoc),
[docbld](https://github.com/Traap/docbld), and
[tlc-article](https://github.com/Traap/tlc-article).

### Supported Systems
**amber** has been tested with cygwin, linux (mint), mingw32/64, Windows 10 and

### Prerequisites
1. [Ruby](https://www.ruby-lang.org/en)
2. [Bundler](https://bundler.io)
3. [Rake](https://github.com/ruby/rake)

### Optional components  are **only** needed if you want to rebuild
[example.pdf](https://github.com/Traap/amber/blob/ReqList/doc/example.pdf)
1. [MiKTeX](https://miktex.org/download)
2. [autodoc](https://github.com/Traap/autodoc)
3. [docbld](https://github.com/Traap/docbld)
4. [tlc-article](https://github.com/Traap/tlc-article.git)

### Working with source
Copy the text below and paste into a shell.

```bash
$ cd $GIT_HOME \
     && git clone https://github.com/traap/amber.git \
            && rake gem:build 
$ cd $GIT_HOME && git clone https://github.com/traap/amber.git
```

#### Run unit test with Rake

```bash
cd $GIT_HOME/amber && rake

```

#### Build and install Amber

```bash
cd $GIT_HOME/amber && rake build:amber
```

#### Validate Amber

```bash
cd $GIT_HOME/amber && rake validate:amber
```

### Input Factory
#### A YAML Plan
```YAML
plan:
  name: A plan name
  purpose:  The purpose of this test plan.
includes:
  - suite:
    - name: a-suite-name
```

#### A YAML Suite
```YAML
suite:
  name: A Suite Name
  purpose:  The purpose of this test suite.
  requirement: 1, 2, and 3.
includes:
  - case:
    - name: a-test-case
    - name: another-test-case
    - name: yet-another-test-case
```
#### A YAML Case
```YAML
case:
  name: First Test Case
  purpose: Demonstrate requirements are met.
  requirement: 1, 2, and 3
  steps:
    - confirm: Program echo has been installed.
      expectation: echo installation location is displayed.
      sudo: true
      command: which
      argument: echo
      evidence: Starts on next line.

    - confirm: Program date has been installed.
      expectation: date installation location is displayed.
      sudo: false
      command: which
      argument: date
      evidence: Starts on next line.

    - confirm: Program man has been installed.
      expectation: man installation location is displayed.
      sudo: false
      command: which
      argument: man
      evidence: Starts on next line.
```

#### Amber directory convention
```bash
../factory
../factory/config
../factory/config/app_config.yaml

../factory
../factory/plan
../factory/plan/a-plan-name/a-plan-name.yaml

../factory/suite
../factory/suite/a-suite-name/a-suite-name.yaml

../factory/case
../factory/case/a-test-case/a-test-case.yaml
../factory/case/another-test-case/another-test-case.yaml
../factory/case/yet-another-test-case/yet-another-test-case.yaml
```
### Output Factory
#### Generated by amber
```bash
../test-output/factory
../test-output/factory/plan
../test-output/factory/plan/a-plan-name/a-plan-name.tex

../test-output/factory/suite
../test-output/factory/suite/a-suite-name/a-suite-name.tex

../test-output/factory/case
../test-output/factory/case/a-test-case/a-test-case.tex
../test-output/factory/case/a-test-case/a-test-case-step-001.tex
../test-output/factory/case/a-test-case/a-test-case-step-step-001-log.tex
../test-output/factory/case/a-test-case/a-test-case-step-step-001-status.tex
../test-output/factory/case/a-test-case/a-test-case-step-step-002-log.tex
../test-output/factory/case/a-test-case/a-test-case-step-step-002-status.tex
../test-output/factory/case/a-test-case/a-test-case-step-step-003-log.tex
../test-output/factory/case/a-test-case/a-test-case-step-step-003-status.tex
```
##### LaTeX test-results output
```bash
\tpo{full/path/to/test-output/factory/plan/a-plan-name/a-plan-nam}
\tso{full/path/to/test-output/factory/suite/a-suite-name/a-suite-nam}
\tco{full/path/to/test-output/factory/case/t001/t001}
\tco{full/path/to/test-output/factory/case/t002/t002}
\tco{full/path/to/test-output/factory/case/t003/t003}
```
##### LaTeX macros
test plan output (tpo), test suite output (tso), and test case output (tco) are
LaTeX macros
[autodoc](https://github.com/traap/autodoc.git) uses
to assemble output from amber into a report.

Refer to [autodoc](https://github.com/traap/autodoc) to learn how an external
program interacts with amber. amber **must** create an output factory containing
files to have them automatically consumed.

#### Generated by amber when --langauge and --browser are used
```bash
../test-output/factory
../test-output/factory/Chrome/fr-eu/plan
../test-output/factory/Chrome/fr-eu/plan/a-plan-name/a-plan-name.tex

../test-output/factory/Chrome/fr-eu/suite
../test-output/factory/Chrome/fr-eu/suite/a-suite-name/a-suite-name.tex

../test-output/factory/Chrome/fr-eu/case
../test-output/factory/Chrome/fr-eu/case/a-test-case/a-test-case.tex
../test-output/factory/Chrome/fr-eu/case/a-test-case/a-test-case-step-001-log.tex
../test-output/factory/Chrome/fr-eu/case/a-test-case/a-test-case-step-001-status.tex
../test-output/factory/Chrome/fr-eu/case/a-test-case/a-test-case-step-002-log.tex
../test-output/factory/Chrome/fr-eu/case/a-test-case/a-test-case-step-002-status.tex
../test-output/factory/Chrome/fr-eu/case/a-test-case/a-test-case-step-002-log.tex
../test-output/factory/Chrome/fr-eu/case/a-test-case/a-test-case-step-002-status.tex
../test-output/factory/Chrome/fr-eu/case/a-test-case/a-test-case-step-003-log.tex
../test-output/factory/Chrome/fr-eu/case/a-test-case/a-test-case-step-003-status.tex
```

##### LaTeX test-results output
```bash
\tpoC{Chrome}{fr-eu}{factory/plan/a-plan-name/a-plan-name}
\tsoC{Chrome}{fr-eu}{factory/suite/a-suite-name/a-suite-nam}
\tcoC{Chrome}{fr-eu}{factory/case/t001/t001}
\tcoC{Chrome}{fr-eu}{factory/case/t002/t002}
\tcoC{Chrome}{fr-eu}{factory/case/t003/t003}
```

##### LaTeX macros
test plan output (tpoC), test suite output (tsoC), and test case output (tcoC)
are LaTeX macros
[autodoc](https://github.com/traap/autodoc) uses to assemble output from amber
into a report.

Refer to
[autodoc](https://github.com/traap/autodoc) to learn how an external program
interacts with amber. amber **must** create an output factory containing files
to have them automatically consumed.

##### Console output
amber captures status, system out, and system error and records the results
to a-test-case.tex file.

##### Step files
amber records PASS or FAIL for each Test Step.  The result is determined by
the program that amber runs.

##### png files
Any custom program amber invokes can take a screen capture and record them
as test-output/factory/case/a-test-case/a-test-case-001.png.  Multiple png
files are supported.

##### csv files
Any custom program amber invokes can record CSV files as
test-output/factory/case/a-test-case/a-test-case-001.csv.  Multiple csv files
are supported.

### amber command line
```bash
amber --help
Usage: amber [options]

Specific options:
    -b, --browser BROWSER            Select Browser
                                     ["None", "Chrome", "Edge", "Firefox", "IE",
                                     "Opera"]
    -n, --nodryrun                   No Dryrun
    -e, --enviornment                List enviornment
    -f, --file x,y,x                 File name
    -l, --language LANGUAGE          Select language
                                     ["zz", "cs", "da", "de", "en", "es", "fr-ca",
                                     "fr-eu", "it", "ne", "no", "pl", "ro", "sv"]
    -v, --verbose                    Verbose
    -L, --log-command                Log Command
    -r, --log-requirement            Log Requirement
    -S, --simulate                   Simulate run to create Test Output
                                     directory.
    -O, --obliterate                 Obliterate Test Output directory before Test
                                     Execution.
    -w, --writer WRITER              Select writer
                                     ["Ascii", "LaTeX"]
    -p, --plan x,y,x                 Plan name
    -s, --suite x,y,x                Suite name
    -c, --case x,y,x                 Case name
    -h, --help                       Show this message
        --version                    Show version
    -d, --dump                       Dump options (must be last).
```

#### General
##### --help
Show this message.

1.5.0.303 is the current version.

#### --verbose
Log the commands what will be run when --nodryrun is used.  **NOTE:** This
option was nurfed because Amber writes the commands to the test output
factory.  This occurs because Amber is capturing standard input, standard
output, and standard error.  Amber conditionally writes either standard output
or standard error to the test output factor based on the exit status of the
program ran.

#### Input Factory
##### --plan
A comma-separated list of test plan names amber is to process.  The following
directory and YAML file name convention is mandatory:
factory/plan/a-plan/a-plan.YAML.

##### --suite
A comma-separated list of test suite names amber is to process.  The
following directory and YAML file name convention is mandatory:
factory/suite/a-suite/a-suite.YAML.

##### --case
A comma-separated list of test case names amber is to process.  The
following directory and YAML file name convention is mandatory:
factory/case/suite/a-case/a-case.YAML.

##### --file
A comma-separated list of file names amber is to process.

#### Substitution
##### --browser
amber uses Chrome by default.  Your Test Plan, Test Suite, and Test Case
must be written to reference a program that uses Web test driver.

##### --language
amber uses english(en) by default.  Your Test Plan, Test Suite, and Test Case
must be written to reference a program the requires internationalization and
localization.

#### Output Factory
##### --log-command
Amber recursively calls Amber.  test-output/commands.log show each Amber
invocation.

##### --log-requirement
test-output/requirements.log is used to record each requirement and YAML
file that references a requirement.

##### --nodryrun
By default, amber does not have side effects when run.  You must explicitlyjkk
use the **--nodryrun** options to cause side effects.  The commands that would
have been executed are echoed to system out.
simulate

##### --simulate
The Simulate option is used to create a Test Output directory so that you can
design your report.  Each command defined in your Test Case YAML file **echo**
echoed to the Test Output directory Test Case file.

##### --obliterate
The Test Output directory is obliterated before running any Tests.

##### --environment
A list of environment variables amber records in the output factory.  See
[environment.rb](https://github.com/Traap/amber/blob/master/lib/amber/cli/environment.rb)
for a complete listing of files.

### Add these functions to .bashrc
```bash
AMBERPATH=${HOME}/git/amber
export AMBERPATH

function newfactoryitem() {
  ${AMBERPATH}/bin/newfactoryitem $@
}

function check-test-output() {
  echo grep -rw --include=\*step*.* test-output/ -e $1
  grep -rw --include=\*step*.* test-output/ -e $1
}

function pass() {
  check-test-output PASS
}

function fail() {
  check-test-output FAIL
}

function requirements() {
  echo grep -rw --include=\*.yaml factory/ -e 'requirement:'
  grep -rw --include=\*.yaml factory/ -e 'requirement:'
}

```

### Amber demonstration
Amber has a Test Input Factory that is used to validate Amber.  The following
commands will demonstrate producing example.pdf.

1. -n, --nodryrun
2. -e, --environment
3. -L, --log-command
4. -r, --log-requirement
6. -O, --obliterate
7. -p, --plan

```
cd git/amber/report
rake validate:amber
```

#### Amber demonstration output
2. git/amber/report/test-output/commands.log
1. git/amber/report/test-output/environment.tex
1. git/amber/report/test-output/requirements.csv
1. git/amber/report/test-output/test-results.tex
1. git/amber/report/_build/exmple.pdf

