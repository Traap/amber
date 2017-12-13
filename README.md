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
            && cd amber \
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
amber --verbose --nodryrun --suite=smoke-test
```

## amber command line
amber --help

Usage: amber [options]

Specific options:
    -n, --nodryrun                   No Dryrun
    -f, --file x,y,x                 File name
    -s, --suite x,y,x                Suite name
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

## --verbose
Echo commands to system output.

## --help
Show this message.
## --version
1.0.0 is this the current version.

# Project Management
The **amber** repository uses a SCRUM framework adapted to standard GitHub
tooling.  **amber** is integrated with Travis-ci.org for continuous
integration and AllanConsulting.slack.com for centralized notification.

Please refer to my [Lightweight Project Management](https://github.com/Traap/lpm)
for the project management strategy I use.
