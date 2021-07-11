# frozen_string_literal: true

require 'amber/version'
require 'amber/cli/browser'
require 'amber/cli/language'
require 'amber/cli/writer'

module Amber
  # Options the user has chosen.
  class Options
    attr_accessor :data

    def initialize
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
      @data[:simulate] = false
      @data[:test_case] = nil
      @data[:test_plan] = nil
      @data[:test_suite] = nil
      @data[:verbose] = false
      @data[:version] = Amber::VERSION
      @data[:writer] = Amber::Writer::DEFAULT
    end

    def browser
      @data[:browser]
    end

    def dryrun?
      @data[:dryrun]
    end

    def dump?
      @data[:dump]
    end

    def filename
      @data[:filename]
    end

    def files
      @data[:files]
    end

    def files?
      @data[:files].nil? ? false : true
    end

    def language
      @data[:language]
    end

    def has_browser?
      @data[:browser].eql?(Amber::Browser::DEFAULT) ? false : true
    end

    def has_language?
      @data[:language].eql?(Amber::Language::DEFAULT) ? false : true
    end

    def log_command?
      @data[:log_command]
    end

    def log_environment?
      @data[:log_environment]
    end

    def log_requirement?
      @data[:log_requirement]
    end

    def obliterate?
      @data[:obliterate]
    end

    def parser
      @data[:parser]
    end

    def run?
      if @data[:simulate]
        true
      elsif !@data[:dryrun]
        true
      else
        false
      end
    end

    def simulate?
      @data[:simulate]
    end

    def test_case
      @data[:test_case]
    end

    def test_plan
      @data[:test_plan]
    end

    def test_suite
      @data[:test_suite]
    end

    def verbose?
      @data[:verbose]
    end

    def version
      @data[:version]
    end

    def writer
      @data[:writer]
    end
  end
end
