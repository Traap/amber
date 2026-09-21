# frozen_string_literal: true

# {{{ Required files

require 'amber/version'
require 'amber/cli/browser'
require 'amber/cli/language'
require 'amber/cli/writer'

# -------------------------------------------------------------------------- }}}
module Amber
  # Options the user has chosen.
  class Options
    # {{{ Attributes

    attr_accessor :data

    # ---------------------------------------------------------------------- }}}
    # {{{ Initialzie

    # rubocop:disable Metrics/AbcSize, Metrics/MethodLength -- legacy option initialization
    def initialize
      # rubocop:enable Metrics/AbcSize, Metrics/MethodLength -- legacy option initialization
      @data = {}
      @data[:browser] = Amber::Browser::DEFAULT
      @data[:dryrun] = true
      @data[:dump] = false
      @data[:filename] = nil
      @data[:files] = nil
      @data[:language] = Amber::Language::DEFAULT
      @data[:log_command] = false
      @data[:log_environment] = false
      @data[:log_requirement] = false
      @data[:obliterate] = false
      @data[:parser] = nil
      @data[:report_dir] = nil
      @data[:simulate] = false
      @data[:test_case] = nil
      @data[:test_plan] = nil
      @data[:test_suite] = nil
      @data[:verbose] = false
      @data[:version] = Amber::VERSION
      @data[:writer] = Amber::Writer::DEFAULT
    end

    # Adapter registry injected by the workflow or populated by the test step.
    def adapter_registry
      @data[:adapter_registry]
    end

    def adapter_registry=(registry)
      @data[:adapter_registry] = registry
    end

    def browser_factory
      @data[:browser_factory]
    end

    def browser_factory=(factory)
      @data[:browser_factory] = factory
    end

    def web_adapter_factory
      @data[:web_adapter_factory]
    end

    def web_adapter_factory=(factory)
      @data[:web_adapter_factory] = factory
    end

    def ocr_engine
      @data[:ocr_engine]
    end

    def ocr_engine=(engine)
      @data[:ocr_engine] = engine
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ report_dir

    def report_dir
      @data[:report_dir]
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ browser

    def browser
      @data[:browser]
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ dryrun?

    def dryrun?
      @data[:dryrun]
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ dump?

    def dump?
      @data[:dump]
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ filename

    def filename
      @data[:filename]
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ files

    def files
      @data[:files]
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ files?

    def files?
      @data[:files].nil? ? false : true
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ language

    def language
      @data[:language]
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ browser?

    def browser?
      @data[:browser].eql?(Amber::Browser::DEFAULT) ? false : true
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ language?

    def language?
      @data[:language].eql?(Amber::Language::DEFAULT) ? false : true
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ log_command?

    def log_command?
      @data[:log_command]
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ log_environment?

    def log_environment?
      @data[:log_environment]
    end
    # ---------------------------------------------------------------------- }}}
    # {{{ log_requirement?

    def log_requirement?
      @data[:log_requirement]
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ obliterate

    def obliterate?
      @data[:obliterate]
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ parser

    def parser
      @data[:parser]
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ run?

    def run?
      if @data[:simulate]
        true
      elsif !@data[:dryrun]
        true
      else
        false
      end
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ simulate?

    def simulate?
      @data[:simulate]
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ test_case

    def test_case
      @data[:test_case]
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ test_plan

    def test_plan
      @data[:test_plan]
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ test_suite

    def test_suite
      @data[:test_suite]
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ verbose?

    def verbose?
      @data[:verbose]
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ version

    def version
      @data[:version]
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ writer

    def writer
      @data[:writer]
    end

    # ---------------------------------------------------------------------- }}}
  end
end
