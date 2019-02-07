# frozen_string_literal: true frozen_string_literal: true
# ------------------------------------------------------------------------------

require 'amber/version'
require 'amber/cli/browser'
require 'amber/cli/language'
require 'amber/cli/writer'
require 'amber/tif/structure/factory_structure'

# ------------------------------------------------------------------------------

module Amber
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
        log_command_option parser
        log_requirement_option parser
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
    def self.log_command_option(parser)
      parser.on('-L', '--log-command', 'Log Command') do |z|
        @clo.options.log_command ^= z
      end
    end

    # --------------------------------------------------------------------------
    def self.log_requirement_option(parser)
      parser.on('-r', '--log-requirement', 'Log Requirement') do |z|
        @clo.options.log_requirement ^= z
      end
    end

    # --------------------------------------------------------------------------
    def self.simulate_option(parser)
      parser.on('-S',
                '--simulate',
                'Simulate run to create Test Output direcotry') do |z|
        @clo.options.simulate ^= z
      end
    end

    # --------------------------------------------------------------------------
    def self.obliterate_option(parser)
      parser.on('-O',
                '--obliterate',
                'Obliterate Test Output directory before Test Execution') do |z|
        @clo.options.obliterate ^= z
      end
    end

    # --------------------------------------------------------------------------
    def self.verbose_option(parser)
      parser.on('-v', '--verbose', 'Verbose') do |z|
        @clo.options.verbose ^= z
      end
    end

    # --------------------------------------------------------------------------
    def self.environment_option(parser)
      parser.on('-e', '--environment', 'List environment') do |z|
        @clo.options.environment ^= z
      end
    end

    # --------------------------------------------------------------------------
    def self.plan_option(parser)
      parser.on('-p', '--plan x,y,x', Array, 'Plan name') do |z|
        @clo.options.filename = z.map! do |a|
          Amber::FactoryStructure.plan_name(a)
        end
      end
    end

    # --------------------------------------------------------------------------
    def self.suite_option(parser)
      parser.on('-s', '--suite x,y,x', Array, 'Suite name') do |z|
        @clo.options.filename = z.map! do |a|
          Amber::FactoryStructure.suite_name(a)
        end
      end
    end

    # --------------------------------------------------------------------------
    def self.case_option(parser)
      parser.on('-c', '--case x,y,x', Array, 'Case name') do |z|
        @clo.options.filename = z.map! do |a|
          Amber::FactoryStructure.case_name(a)
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
        puts "Command Line Option values:"
        puts "    browser: #{@clo.options.browser}"
        puts "     dryrun: #{@clo.options.dryrun}"
        puts "environment: #{@clo.options.environment}"
        puts "   filename: #{@clo.options.filename}"
        puts "   language: #{@clo.options.language}"
        puts "log-command: #{@clo.options.log_command}"
        puts " obliterate: #{@clo.options.obliterate}"
        puts "requirement: #{@clo.options.requirement}"
        puts "   simulate: #{@clo.options.simulate}"
        puts "    verbose: #{@clo.options.verbose}"
        puts "     writer: #{@clo.options.writer}"
        puts "Command Line Option function values:"
        puts "             okay_to_run?: #{@clo.options.okay_to_run?}"
        puts "      okay_to_obliterate?: #{@clo.options.okay_to_obliterate?}"
        puts "okay_to_list_requirement?: #{@clo.options.okay_to_list_requirement?}"
        puts "     okay_to_log_command?: #{@clo.options.okay_to_log_command?}"
        puts "            has_language?: #{@clo.options.has_language?}"
        puts "             has_browser?: #{@clo.options.has_browser?}"
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
