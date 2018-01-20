module Amber 
  class Include < Node 

    def initialize(data, options)
      super("Include", data, options)
    end

    def echo_to_sysout; end

    def run_command 
      @data.each do |k,v|
        case k
        when "plan"
          include_this_file v, "--plan"
        when "suite"
          include_this_file v, "--suite"
        when "case"
          include_this_file v, "--case"
        when "file"
          include_this_file v, "--file"
        end
      end
    end

    def include_this_file(name, opt)
      opt_and_files = "#{opt}=#{name.map{|n| n.values}.join(',')}"
      @command ="amber #{opt_and_files}"
      @command.concat " --nodryrun" if !@options.dryrun
      method(:run_command).super_method.call
    end

  end # Include
end # Amber