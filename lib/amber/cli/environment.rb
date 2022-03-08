# frozen_string_literal: true

module Amber
  # System Enviornment values written to output file.
  class Environment
    # {{{ Attributes

    attr_reader :environment

    # ---------------------------------------------------------------------- }}}
    # {{{ Initilize

    def initialize
      @environment = [
        'ALLUSERSPROFILE',
        'APPDATA',
        'CLASSPATH',
        'COMPUTERNAME',
        'COMSPEC',
        'FP_NO_HOST_CHECK',
        'GIT_BRANCH',
        'HOME',
        'HOMEDRIVE',
        'HOMEPATH',
        'HOSTNAME',
        'JRE_HOME',
        'LANG',
        'LOCALAPPDATA',
        'LOGONSERVER',
        'NUMBER_OF_PROCESSORS',
        'OLDPWD',
        'OS',
        'PATH',
        'PRINTER',
        'PROCESSOR_ARCHITECTURE',
        'PROCESSOR_IDENTIFIER',
        'PROCESSOR_LEVEL',
        'PROCESSOR_REVISION',
        'PROFILEREAD',
        'ProgramData',
        'PROGRAMFILES',
        'ProgramFiles(x86)',
        'ProgramW6432',
        'PSModulePath',
        'PUBLIC',
        'PWD',
        'SESSIONNAME',
        'SHELL',
        'SHLVL',
        'SYSTEMDRIVE',
        'SYSTEMROOT',
        'TEMP',
        'TMP',
        'TZ',
        'USER',
        'USERDNSDOMAIN',
        'USERDOMAIN',
        'USERNAME',
        'USERPROFILE',
        'WINDIR'
      ].freeze
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ echo_to_sysout

    def echo_to_sysout
      puts "\nSystem Environment"
      @environment.each do |env|
        if env == 'PATH'
          echo_e_to_sysout(env)
        else
          puts "  #{env} = #{ENV[env]}"
        end
      end
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ echo_to_sysout

    def echo_e_to_sysout(env)
      puts "  #{env} = "
      part = ENV[env].split(':')
      part.each do |name|
        puts "         #{name}"
      end
    end

    # ---------------------------------------------------------------------- }}}
  end
end
