# frozen_string_literal: true

# {{{ required items.

require 'amber/version'
require 'amber/cli/browser'
require 'amber/cli/language'
require 'amber/cli/writer'
require 'amber/tif/structure/factory_structure'

# -------------------------------------------------------------------------- }}}
# {{{ Amber Command Line Options
module Amber
  class CommandLineOptions
    # {{{ attributes

    attr_accessor :clo, :options

    # ---------------------------------------------------------------------- }}}
    # {{{ Initialize CommandLineOptions

    def initialize
      @options = Options.new
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ concat_files

    # rubocop:disable Metrics.BlockLength
    def concat_files
      # rubocop:enable Metrics.BlockLength

      # Concatenate test_plan, test_suite, test_case, and filename into
      # @clo.options[:files] only once.

      return unless @options.data[:files].nil?

      files = []
      files.concat(@options.data[:test_plan])  unless @options.data[:test_plan].nil?
      files.concat(@options.data[:test_suite]) unless @options.data[:test_suite].nil?
      files.concat(@options.data[:test_case])  unless @options.data[:test_case].nil?
      files.concat(@options.data[:filename])   unless @options.data[:filename].nil?
      @options.data[:files] = files            unless files.none?
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ Parse ARGV and Options.

    def self.parse(argv)
      @clo = CommandLineOptions.new
      option_parser.parse! argv
      @clo.concat_files
      @clo.options
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ Options Parser populates options sructure.

    # rubocop:disable Metrics.AbcLength
    def self.option_parser
      # rubocop:enable Metrics.AbcLength

      @option_parser ||= OptionParser.new do |opts|
        opts.banner = 'Usage: amber [argv] [options]'
        opts.separator ''
        opts.separator 'Specific options:'

        browser opts
        dryrun opts
        filename opts
        help opts
        language opts
        verbose opts
        log_command opts
        log_environment opts
        log_requirement opts
        simulate opts
        obliterate opts
        version opts
        writer opts

        test_plan opts
        test_suite opts
        test_case opts

        dump opts
      end
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ Help option.

    def self.help(opts)
      opts.on_tail('-h', '--help', 'Show this message') do
        puts opts
        exit
      end
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ Dryrun option.

    def self.dryrun(opts)
      opts.on('-n', '--nodryrun', 'No Dryrun') do |z|
        @clo.options.data[:dryrun] ^= z
      end
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ Log Command option.

    def self.log_command(opts)
      opts.on('-L', '--log-command', 'Log Command') do |z|
        @clo.options.data[:log_command] ^= z
      end
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ Log Environment option.

    def self.log_environment(opts)
      opts.on('-e', '--log-environment', 'List environment') do |z|
        @clo.options.data[:log_environment] = z
      end
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ Log Requirement option.

    def self.log_requirement(opts)
      opts.on('-r', '--log-requirement', 'Log Requirement') do |z|
        @clo.options.data[:log_requirement] = z
      end
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ Simulate option.

    def self.simulate(opts)
      opts.on('-S',
              '--simulate',
              'Simulate run to create Test Output direcotry') do |z|
        @clo.options.data[:simulate] = z
      end
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ Obliterate option.

    def self.obliterate(opts)
      opts.on('-O',
              '--obliterate',
              'Obliterate Test Output directory before Test Execution') do |z|
        @clo.options.data[:obliterate] = z
      end
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ Verbose option.

    def self.verbose(opts)
      opts.on('-v', '--verbose', 'Verbose') do |z|
        @clo.options.data[:verbose] = z
      end
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ Test Plan option.

    def self.test_plan(opts)
      opts.on('-p', '--plan x,y,z', Array, 'Plan name') do |z|
        @clo.options.data[:test_plan] = z.map! do |a|
          Amber::FactoryStructure.plan_name(a)
        end
      end
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ Test Suite option.

    def self.test_suite(opts)
      opts.on('-s', '--suite x,y,z', Array, 'Suite name') do |z|
        @clo.options.data[:test_suite] = z.map! do |a|
          Amber::FactoryStructure.suite_name(a)
        end
      end
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ Test Case option.

    def self.test_case(opts)
      opts.on('-c', '--case x,y,z', Array, 'Case name') do |z|
        @clo.options.data[:test_case] = z.map! do |a|
          Amber::FactoryStructure.case_name(a)
        end
      end
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ Filename option.

    def self.filename(opts)
      opts.on('-f', '--file x,y,x', Array, 'File name') do |z|
        @clo.options.data[:filename] = z.map!(&:to_s)
      end
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ Writer option.

    def self.writer(opts)
      opts.on('-w', '--writer WRITER',
              String, Amber::Writer::NAMES,
              'Select writer', Amber::Writer::NAMES.to_s) do |z|
        @clo.options.data[:writer] = z
      end
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ Browser option.

    def self.browser(opts)
      opts.on('-b', '--browser BROWSER',
              String, Amber::Browser::NAMES,
              'Select Browser', Amber::Browser::NAMES.to_s) do |z|
        @clo.options.data[:browser] = z
      end
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ Language option.

    def self.language(opts)
      opts.on('-l', '--language LANGUAGE',
              Amber::Language::NAMES, Amber::Language::CODE,
              'Select language', Amber::Language::NAMES.to_s) do |z|
        @clo.options.data[:language] = z
      end
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ Dump option.

    def self.dump(opts)
      opts.on_tail('-d', '--dump', 'Dump ARGV, Options, and exit.') do |z|
        @clo.options.data[:dump] = z
      end
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ Version option.

    def self.version(opts)
      opts.on_tail('--version', 'Show version') do
        puts @clo.options.data[:version]
      end
    end

    # ---------------------------------------------------------------------- }}}
  end
end
# -------------------------------------------------------------------------- }}}
