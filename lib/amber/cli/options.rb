# frozen_string_literal: true

require 'amber/version'
require 'amber/cli/browser'
require 'amber/cli/language'
require 'amber/cli/writer'

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

end