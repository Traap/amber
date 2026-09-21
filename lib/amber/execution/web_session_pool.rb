# frozen_string_literal: true

require 'amber/execution/web_session'

module Amber
  module Execution
    # Owns one browser for a group of web cases and creates isolated case views.
    class WebSessionPool
      def initialize(browser_factory:)
        @browser_factory = browser_factory
        @browser = nil
        @browser_name = nil
        @configuration = nil
      end

      # rubocop:disable Metrics/MethodLength -- creates a case-scoped session view
      def session(browser:, configuration: {}, output_directory: nil)
        start(browser, configuration)
        case_session = WebSession.new(
          browser_factory: @browser_factory,
          browser: browser,
          configuration: configuration,
          output_directory: output_directory,
          shared_browser: @browser
        )
        case_session.reset if @browser_name_was_set
        case_session.prepare_case(output_directory)
        @browser_name_was_set = true
        case_session
      end
      # rubocop:enable Metrics/MethodLength

      # rubocop:disable Metrics/MethodLength -- always releases the pooled browser
      def close
        return unless @browser

        if @browser.respond_to?(:quit)
          @browser.quit
        elsif @browser.respond_to?(:close)
          @browser.close
        end
      ensure
        @browser = nil
        @browser_name = nil
        @configuration = nil
        @browser_name_was_set = false
      end
      # rubocop:enable Metrics/MethodLength

      private

      def start(browser, configuration)
        return if compatible?(browser, configuration)

        close if @browser
        @browser_name = browser.to_s
        @configuration = configuration.dup
        @browser = @browser_factory.start(@browser_name, @configuration)
      end

      def compatible?(browser, configuration)
        @browser && @browser_name == browser.to_s && @configuration == configuration
      end
    end
  end
end
