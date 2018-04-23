module Amber
  class Environment
    attr_reader :environment

    def initialize
      @environment = [
        'ALLUSERSPROFILE', 'APPDATA', 'CLASSPATH', 'COMPUTERNAME', 'COMSPEC',
        'FP_NO_HOST_CHECK', 'GIT_BRANCH', 'HOME', 'HOMEDRIVE', 'HOMEPATH',
        'HOSTNAME', 'JRE_HOME', 'LANG', 'LOCALAPPDATA', 'LOGONSERVER',
        'NUMBER_OF_PROCESSORS', 'OLDPWD', 'OS', 'PATH', 'PRINTER',
        'PROCESSOR_ARCHITECTURE', 'PROCESSOR_IDENTIFIER', 'PROCESSOR_LEVEL',
        'PROCESSOR_REVISION', 'PROFILEREAD', 'ProgramData', 'PROGRAMFILES',
        'ProgramFiles(x86)', 'ProgramW6432', 'PSModulePath', 'PUBLIC', 'PWD',
        'SESSIONNAME', 'SHELL', 'SHLVL', 'SYSTEMDRIVE', 'SYSTEMROOT', 'TEMP',
        'TMP', 'TZ', 'USER', 'USERDNSDOMAIN', 'USERDOMAIN', 'USERNAME',
        'USERPROFILE', 'WINDIR'
      ]
    end

    def echo_to_sysout
      puts "\nSystem Environment"
      @environment.each do |e|
        if e == 'PATH'
          echo_e_to_sysout(e)
        else
          puts "  #{e} = #{ENV[e]}"
        end
      end
    end

    def echo_e_to_sysout(e)
      puts "  #{e} = "
      f = ENV[e].split(':')
      f.each do |g|
        puts "         #{g}"
      end
    end
  end
end
