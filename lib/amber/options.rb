require 'amber/version'
require 'amber/browser'
require 'amber/language'
require 'amber/writer'

module Amber
  # Options the user has chosen.
  class Options
    attr_accessor :browser, :dryrun, :environment, :filename, :language, 
                  :parser, :obliterate, :simulate, :verbose, :version, :writer
    def initialize
      @browser = Amber::Browser::DEFAULT
      @dryrun = true
      @environment = false
      @filename = []
      @language = Amber::Language::DEFAULT
      @parser = nil
      @obliterate = false 
      @simulate = false
      @verbose = false
      @version = Amber::VERSION
      @writer = Amber::Writer::DEFAULT
    end

    def okay_to_run?
      if @simulate
        true
      elsif !@dryrun
        true
      else
        false
      end
    end

    def okay_to_echo_env?
      @environment && okay_to_run?
    end

    def okay_to_obliterate?
      @obliterate
    end
    
    def has_language?
      @language.eql?(Amber::Language::DEFAULT) ? false : true 
    end

    def has_browser?
      @browser.eql?(Amber::Browser::DEFAULT) ? false : true 
    end

  end

  # Command Line Options 
  class CommandLineOptions
    attr_accessor :clo, :options

    # --------------------------------------------------------------------------
    def initialize
      @options = Options.new
    end

    # --------------------------------------------------------------------------
    def self.parse(args)
      @clo = CommandLineOptions.new
      option_parser.parse! args
      @clo.options
    end

    # --------------------------------------------------------------------------
    def self.option_parser
      @parser ||= OptionParser.new do |parser|
        parser.banner = 'Usage: amber [options]'
        parser.separator ''
        parser.separator 'Specific options:'

        browser_option parser
        dryrun_option parser
        environment_option parser
        file_option parser
        help_option parser
        language_option parser
        verbose_option parser
        simulate_option parser
        obliterate_option parser
        version_option parser
        writer_option parser

        plan_option parser
        suite_option parser
        case_option parser

        dump_option parser
      end
    end

    # --------------------------------------------------------------------------
    def self.help_option(parser)
      parser.on_tail('-h', '--help', 'Show this message') do
        puts parser
        exit
      end
    end

    # --------------------------------------------------------------------------
    def self.dryrun_option(parser)
      parser.on('-n', '--nodryrun', 'No Dryrun') do |z|
        @clo.options.dryrun ^= z
      end
    end
    
    # --------------------------------------------------------------------------
    def self.simulate_option(parser)
      parser.on('-S', 
                '--simulate', 
                'Simulate run to create Test Output direcotry') do |z|
        @clo.options.simulate = z
      end
    end

    # --------------------------------------------------------------------------
    def self.obliterate_option(parser)
      parser.on('-O', 
                '--obliterate', 
                'Obliterate Test Output directory before Test Execution') do |z|
        @clo.options.obliterate = z
      end
    end

    # --------------------------------------------------------------------------
    def self.verbose_option(parser)
      parser.on('-v', '--verbose', 'Verbose') do |z|
        @clo.options.verbose = z
      end
    end

    # --------------------------------------------------------------------------
    def self.environment_option(parser)
      parser.on('-e', '--environment', 'List environment') do |z|
        @clo.options.environment = z
      end
    end

    # --------------------------------------------------------------------------
    def self.plan_option(parser)
      parser.on('-p', '--plan x,y,x', Array, 'Plan name') do |z|
        @clo.options.filename = z.map! do |a|
          "factory/plan/#{a}/#{a}.yaml"
        end
      end
    end

    # --------------------------------------------------------------------------
    def self.suite_option(parser)
      parser.on('-s', '--suite x,y,x', Array, 'Suite name') do |z|
        @clo.options.filename = z.map! do |a|
          "factory/suite/#{a}/#{a}.yaml"
        end
      end
    end

    # --------------------------------------------------------------------------
    def self.case_option(parser)
      parser.on('-c', '--case x,y,x', Array, 'Case name') do |z|
        @clo.options.filename = z.map! do |a|
          "factory/case/#{a}/#{a}.yaml"
        end
      end
    end

    # --------------------------------------------------------------------------
    def self.file_option(parser)
      parser.on('-f', '--file x,y,x', Array, 'File name') do |z|
        @clo.options.filename = z.map!(&:to_s)
      end
    end

    # --------------------------------------------------------------------------
    def self.writer_option(parser)
      parser.on('-w', '--writer WRITER', String, Amber::Writer::NAMES,
                'Select writer', Amber::Writer::NAMES.to_s) do |z|
        @clo.options.writer = z
      end
    end

    # --------------------------------------------------------------------------
    def self.browser_option(parser)
      parser.on('-b', '--browser BROWSER', String, Amber::Browser::NAMES,
                'Select Browser', Amber::Browser::NAMES.to_s) do |z|
        @clo.options.browser = z
      end
    end

    # --------------------------------------------------------------------------
    def self.language_option(parser)
      parser.on('-l', '--language LANGUAGE',
                Amber::Language::NAMES, Amber::Language::CODE,
                'Select language', Amber::Language::NAMES.to_s) do |z|
        @clo.options.language = z
      end
    end

    # --------------------------------------------------------------------------
    def self.dump_option(parser)
      parser.on_tail('-d', '--dump', 'Dump options (must be last).') do
        puts "    browser: #{@clo.options.browser}"
        puts "     dryrun: #{@clo.options.dryrun}"
        puts "environment: #{@clo.options.environment}"
        puts "   filename: #{@clo.options.filename}"
        puts "   language: #{@clo.options.language}"
        puts " obliterate: #{@clo.options.obliterate}"
        puts "   simulate: #{@clo.options.simulate}"
        puts "    verbose: #{@clo.options.verbose}"
        puts "     writer: #{@clo.options.writer}"
        exit
      end
    end

    # --------------------------------------------------------------------------
    def self.version_option(parser)
      parser.on_tail('--version', 'Show version') do
        puts @clo.options.version 
      end
    end

    # --------------------------------------------------------------------------
  end
end
