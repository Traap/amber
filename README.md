### Automated testing 
**amber** is a Ruby gem that is used for automated testing.  **amber** was designed
to automate various types of testing that require creating and assembling
detailed records intended to prove a product or tool has been tested for its
intended purpose.  **amber** borrows from a Ruby-on-Rails concept of convention
over configuration.  In this regard, amber consumes YAML documents that are
placed beneath a factory directory root.  You are encouraged to review
**amber's** report directory because it has been designed to demonstrate
**amber's** capabilities.  [example.pdf](https://bitbucket-vial.intra.fresenius.com/projects/SOUP/repos/amber/browse/doc/example.pdf) 
has been provided to demonstrate the automation framework components amber,
[autodoc](https://bitbucket-vial.intra.fresenius.com/scm/soup/autodoc.git),
[docbld](https://bitbucket-vial.intra.fresenius.com/scm/soup/docbld.git), and
[tlc-article](https://bitbucket-vial.intra.fresenius.com/scm/soup/tlc-article.git).


### Supported Systems
**amber** has been tested with cygwin, linux (mint), mingw32, and wsl.

### Prerequisites 
1. [Ruby](https://www.ruby-lang.org/en)
2. [Bundler](https://bundler.io)
3. [Rake](https://ruby/rake)

### Installing amber
```bash
gem install amber
```

### Optional components  are **only** needed if you want to rebuild [example.pdf](https://bitbucket-vial.intra.fresenius.com/projects/SOUP/repos/amber/browse/doc/example.pdf) 
1. [MiKTeX](https://miktex.org/download)
2. [autodoc](https://bitbucket-vial.intra.fresenius.com/scm/soup/autodoc.git)
3. [docbld](https://bitbucket-vial.intra.fresenius.com/scm/soup/docbld.git)
4. [tlc-article](https://bitbucket-vial.intra.fresenius.com/scm/soup/tlc-article.git)

### Working with source 
Copy the text below and paste into a shell.  The commands will:
1. Change directory to home
2. Clone amber
3. Move to amber directory
4. Download dependencies
5. Run unit test
5. Build and install local gem
6. Run amber and display amber's current version.

```bash
$ cd $HOME \
     && git clone https://bitbucket-vial.intra.fresenius.com/scm/soup/amber.git \
            & cd amber \
            && bundle install \
            && bundle exec rake \
            && bundle exec rake install
                && amber --version
            
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
../factory/config/app_config.YAML

../factory
../factory/plan
../factory/plan/a-plan-name/a-plan-name.YAML

../factory/suite
../factory/suite/a-suite-name/a-suite-name.YAML

../factory/case
../factory/case/a-test-case/a-test-case.YAML
../factory/case/another-test-case/another-test-case.YAML
../factory/case/yet-another-test-case/yet-another-test-case.YAML
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
../test-output/factory/case/a-test-case/step-001.tex
../test-output/factory/case/a-test-case/step-002.tex
../test-output/factory/case/a-test-case/step-002.tex
../test-output/factory/case/a-test-case/step-003.tex
```
##### LaTeX test-results output
```bash
\tpo{unit-test}
\tso{unit-test}
\tco{t001}
\tco{t002}
\tco{t003}
```
##### LaTeX macros
test plan output (tpo), test suite output (tso), and test case output (tco) are
LaTeX macros [autodoc](https://bitbucket-vial.intra.fresenius.com/scm/soup/autodoc.git) uses to assemble output
from amber into a report.

Refer to [autodoc](https://bitbucket-vial.intra.fresenius.com/scm/soup/autodoc.git) to learn how an external
program amber runs **must** create output factor files to have them automatically
consumed.

#### Generated by amber when --langauge and --browser are used
```bash
../test-output/factory
../test-output/factory/Chrome/fr-eu/plan
../test-output/factory/Chrome/fr-eu/plan/a-plan-name/a-plan-name.tex

../test-output/factory/Chrome/fr-eu/suite
../test-output/factory/Chrome/fr-eu/suite/a-suite-name/a-suite-name.tex

../test-output/factory/Chrome/fr-eu/case
../test-output/factory/Chrome/fr-eu/case/a-test-case/a-test-case.tex
../test-output/factory/Chrome/fr-eu/case/a-test-case/step-001.tex
../test-output/factory/Chrome/fr-eu/case/a-test-case/step-002.tex
../test-output/factory/Chrome/fr-eu/case/a-test-case/step-002.tex
../test-output/factory/Chrome/fr-eu/case/a-test-case/step-003.tex
```

##### LaTeX test-results output
```bash
\tpoC{Chrome}{fr-eu}{unit-test}
\tsoC{Chrome}{fr-eu}{unit-test}
\tcoC{Chrome}{fr-eu}{t001}
\tcoC{Chrome}{fr-eu}{t002}
\tcoC{Chrome}{fr-eu}{t003}
```

##### LaTeX macros
test plan output (tpoC), test suite output (tsoC), and test case output (tcoC)
are LaTeX macros [autodoc](https://bitbucket-vial.intra.fresenius.com/scm/soup/autodoc.git) uses to assemble
output from amber into a report.

Refer to [autodoc](https://bitbucket-vial.intra.fresenius.com/scm/soup/autodoc.git) to learn how an external
program amber runs **must** create output factor files to have them automatically
consumed.

##### Console output
**amber** captures status, system out, and system error and records the results
to a-test-case.tex file.  

##### Step files
**amber** records PASS or FAIL for each Test Step.  The result is determined by
the program that **amber** runs.

##### png files
Any custom program **amber** invokes can take a screen capture and record them 
as test-output/factory/case/a-test-case/a-test-case-001.png.  Multiple png 
files are supported.

##### csv files
Any custom program **amber** invokes can record CSV files as
test-output/factory/case/a-test-case/a-test-case-001.csv.  Multiple csv 
files are supported.

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

##### --version
1.2.167 is the current version.

#### --verbose
Log the commands what will be run when --nodryrun is used.  **NOTE:** This
option write nurfed because amber writes the commands to the test output
factory.  This occurs because amber is capturing standard input, standard
output, and standard error.  Amber conditionally writes either standard output
or standard error to the test output factor based on the exit status of the
program ran.

#### Input Factory
##### --plan
A comma-separated list of test plan names **amber** is to process.  The following
directory and YAML file name convention is mandatory:
factory/plan/a-plan/a-plan.YAML.

##### --suite
A comma-separated list of test suite names **amber** is to process.  The
following directory and YAML file name convention is mandatory:
factory/suite/a-suite/a-suite.YAML.

##### --case
A comma-separated list of test case names **amber** is to process.  The
following directory and YAML file name convention is mandatory:
factory/case/suite/a-case/a-case.YAML.

##### --file
A comma-separated list of file names **amber** is to process.

#### Substitution
##### --browser
**amber** uses Chrome by default.  Your Test Plan, Test Suite, and Test Case
must be written to reference a program that uses Web test driver.

##### --language
**amber** uses english(en) by default.  Your Test Plan, Test Suite, and Test Case must be
written to reference a program the requires internationalization and
localization.

#### Output Factory
##### --nodryrun
By default, **amber** does not have side effects when run.  You must explicitly
use the **--nodryrun** options to cause side effects.  The commands that would
have been executed are echoed to system out.

##### --simulate
The Simulate option is used to create a Test Output directory so that you can
design your report.  Each command defined in your Test Case YAML file **echo**
echoed to the Test Output directory Test Case file.

##### --obliterate
The Test Output directory is obliterated before running any Tests.

##### --environment
A list of environment variables **amber** records in the output factory.  See
[environment.rb](https://bitbucket-vial.intra.fresenius.com/projects/SOUP/repos/amber/browse/lib/amber/environment.rb)
for a complete listing of files.

### Add these functions to .bashrc
```bash
AMBERPATH=${HOME}/git/amber
export AMBERPATH

function newfactoryitem() {
  ${AMBERPATH}/bin/newfactoryitem $@
}

function check-test-output() {
  echo grep -rw --include=\s*.* test-output/ -e $1
  grep -rw --include=\s*.* test-output/ -e $1
}

function pass() {
  check-test-output PASS
}

function fail() {
  check-test-output FAIL
}
```

### Project Management
The **amber** repository uses a SCRUM framework adapted to standard GitHub
tooling.  **amber** is integrated with Travis-ci.org for continuous
integration and AllanConsulting.slack.com for centralized notification.

Please refer to my [Lightweight Project Management](https://github.com/Traap/lpm)
for the project management strategy I use.
