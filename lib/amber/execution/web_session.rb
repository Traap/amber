# frozen_string_literal: true

require 'amber/execution/evidence_collector'

module Amber
  module Execution
    # Browser lifecycle boundary used by the web adapter.
    class WebSession
      attr_reader :browser, :browser_name, :configuration, :evidence

      def initialize(browser_factory:, browser:, configuration: {}, evidence: nil)
        @browser_factory = browser_factory
        @browser_name = browser.to_s
        @configuration = configuration.dup.freeze
        @evidence = evidence || EvidenceCollector.new
        @browser = nil
      end

      def start
        return @browser if @browser

        @browser = @browser_factory.start(@browser_name, @configuration)
      end

      def close
        return unless @browser

        if @browser.respond_to?(:quit)
          @browser.quit
        elsif @browser.respond_to?(:close)
          @browser.close
        end
      ensure
        @browser = nil
      end
    end
  end
end
