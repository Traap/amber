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

      def session(browser:, configuration: {}, output_directory: nil)
        configure(browser, configuration)
        reset if @browser
        WebSession.new(
          browser_factory: @browser_factory,
          browser: browser,
          configuration: configuration,
          output_directory: output_directory,
          shared_pool: self
        )
      end

      attr_reader :browser

      def start
        return @browser if @browser

        @browser = @browser_factory.start(@browser_name, @configuration)
      end

      def close_browser
        return unless @browser

        quit_browser
      ensure
        @browser = nil
      end

      def new_browser
        raise ArgumentError, 'Cannot start a new browser while one is active; close_browser first' if @browser

        start
      end

      def close
        close_browser
      ensure
        @browser = nil
        @browser_name = nil
        @configuration = nil
      end

      private

      def configure(browser, configuration)
        raise ArgumentError, 'Cannot change browser while one is active; close_browser first' if
          @browser && !compatible?(browser, configuration)

        @browser_name = browser.to_s
        @configuration = configuration.dup
      end

      def reset
        WebSession.new(browser_factory: @browser_factory, browser: @browser_name,
                       configuration: @configuration, shared_browser: @browser).reset
      end

      def quit_browser
        if @browser.respond_to?(:quit)
          @browser.quit
        elsif @browser.respond_to?(:close)
          @browser.close
        end
      end

      def compatible?(browser, configuration)
        @browser && @browser_name == browser.to_s && @configuration == configuration
      end
    end
  end
end
