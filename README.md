**amber** is a Ruby gem that is used automated testing.  **amber** has been
tested with cygwin, linix, linux (mint), and wsl.

# Prerequisites 
1. git client
2. ruby 

# Installation
## Using the gem
```bash
$ CD $HOME
$ gem install amber
$ gem --verbose --suite=smoke-test
```

## Working with source 
Copy the text below and paste into a shell.  The commands will:
1. Change directory to home
2. Clone Traap/amber
3. Move to amber directory
4. Download dependencies
5. Run unit test
5. Build and install local gem
6. Run amber and display amber's current version.

```bash
$ cd $HOME \
     && git clone http://github.com/Traap/amber.git \
            ear
            & cd amber \
            && bundle install \
            && bundle exec rake \
            && bundle exec rake install
            && amber --verbose
```

# Traap/amber-computer
[amber-computer](https://github.com/Traap/amber-computer) repository has app
(applications) and bundles **amber** knows how to install.

## Install and configure Vim
```bash
amber --suite=smoke-test
```

## amber command line
amber --help

Usage: amber [options]

Specific options:
    -n, --nodryrun                   No Dryrun
    -f, --file x,y,x                 File name
    -s, --plan x,y,x                 Plan name 
    -s, --suite x,y,x                Suite name
    -s, --case x,y,x                 Name name
    -v, --verbose                    Verbose
    -h, --help                       Show this message
        --version                    Show version

## --nodryrun
By default, **amber** does not run a test suite.  You must explicitly
use the **--nodryrun** options to cause side effects.  The commands that would
have been executed are echoed to system out.

## --suite
A comma-separated list of test suite names **amber** is to process.  The following
directory and YAML file name convention is mandatory:
suite/a-suite/a-suite.yaml.

## --file
A comma-separated list of file names **amber** is to process.

## --help
Show this message.

## --version
0.0.36 is this the current version.

# Validating a tool
Amber automates creating and assembling detailed records intended to provide
a product or tool has been tested for its intended purpose.  Amber borrows from
a Ruby-on-Rails convention of convention over configuration.  In this regard,
amber consumes yaml documents that are placed beneath a factory directory root.
You are encouraged to review Amber's factory directory because it has been
designed to demonstrate Amber's capabilities.

## A yaml Plan
```yaml
plan:
  name: A plan name
  purpose:  The purpose of this test plan.
includes:
  - suite:
    - name: a-suite-name 
```

## A yaml Suite
```yaml
suite:
  name: A Suite Name 
  purpose:  The purpose of this test suite.
  requirement: 1, 2, 3, and 4.
includes:
  - case:
    - name: a-test-case 
    - name: another-test-case 
    - name: yet-another-test-case 
```

## Amber directory convention
```bash
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

# Project Management
The **amber** repository uses a SCRUM framework adapted to standard GitHub
tooling.  **amber** is integrated with Travis-ci.org for continuous
integration and AllanConsulting.slack.com for centralized notification.

Please refer to my [Lightweight Project Management](https://github.com/Traap/lpm)
for the project management strategy I use.
