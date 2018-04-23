require 'amber/language'
require 'amber/test'

module Amber
  # A YAML directive to include another file.
  class Include < Test
    def initialize(data, options)
      super(nil, 'Include', data, options)
    end

    def echo_to_sysout; end

    def run_command
      @data.each do |k, v|
        case k
        when 'plan'
          include_this_file v, '--plan'
        when 'suite'
          include_this_file v, '--suite'
        when 'case'
          include_this_file v, '--case'
        when 'file'
          include_this_file v, '--file'
        end
      end
    end

    def include_this_file(name, opt)
      code = Amber::Language::CODE.key(@options.language)
      opt_and_files = "#{opt}=#{name.map(&:values).join(',')}"
      @command = 'amber'
      @command.concat ' --nodryrun'                    unless @options.dryrun
      @command.concat " --writer=#{@options.writer}"   unless @options.writer.nil?
      @command.concat " --browser=#{@options.browser}" unless @options.browser.nil?
      @command.concat " --language=#{code}"            unless code.nil?
      @command.concat " #{opt_and_files}"
      method(:run_command).super_method.call
    end
  end
end
