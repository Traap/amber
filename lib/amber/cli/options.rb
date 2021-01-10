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
      @data[:environment] = false
      @data[:filename] = []
      @data[:files] = []
      @data[:language] = Amber::Language::DEFAULT
      @data[:log_command] = false 
      @data[:log_requirement] = false
      @data[:obliterate] = false
      @data[:parser] = nil
      @data[:simulate] = false
      @data[:test_case] = []
      @data[:test_plan] = []
      @data[:test_suite] = []
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

    def environment
      @data[:environment]
    end

    def filename
      @data[:filename]
    end

    def files
      @data[:files]
    end

    def language
      @data[:language]
    end

    def has_language?
      @data[:language].eql?(Amber::Language::DEFAULT) ? false : true
    end

    def has_browser?
      @data[:browser].eql?(Amber::Browser::DEFAULT) ? false : true
    end
    def log_command
      @data[:log_command]
    end

    def log_requirement
      @data[:log_requirement]
    end

    def obliterate?
      @data[:obliterate]
    end

    def okay_to_run?
      if @data[:simulate]
        true
      elsif !@data[:dryrun]
        true
      else
        false
      end
    end

    def okay_to_echo_env?
      @data[:environment] && okay_to_run?
    end

    def okay_to_log_command?
      @data[:log_command]
    end

    def okay_to_log_requirement?
      @data[:log_requirement]
    end

    def okay_to_obliterate?
      @data[:obliterate]
    end

    def parser 
      @data[:parser]
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
