# frozen_string_literal: true

require 'fileutils'
require 'amber/execution/evidence_collector'

module Amber
  module Execution
    # Browser lifecycle boundary used by the web adapter.
    class WebSession
      attr_reader :browser_name, :configuration, :evidence, :output_directory

      # rubocop:disable Metrics/MethodLength -- initializes standalone and pooled session state
      def initialize(browser_factory:, browser:, configuration: {}, evidence: nil, output_directory: nil,
                     shared_browser: nil, shared_pool: nil)
        @browser_factory = browser_factory
        @browser_name = browser.to_s
        @shared_browser = shared_browser
        @shared_pool = shared_pool
        @browser = shared_browser
        @owns_browser = shared_browser.nil? && shared_pool.nil?
        @output_directory = output_directory && File.expand_path(output_directory)
        @configuration = configuration.dup
        @configuration[:download_path] ||= @output_directory if @output_directory
        @configuration.freeze
        @evidence = evidence || EvidenceCollector.new
      end
      # rubocop:enable Metrics/MethodLength

      def browser
        @shared_pool ? @shared_pool.browser : @browser
      end

      def evidence_path(path)
        raise ArgumentError, 'Evidence path must be relative' if Pathname.new(path.to_s).absolute?

        root = @output_directory || Dir.pwd
        candidate = File.expand_path(path, root)
        prefix = "#{root}#{File::SEPARATOR}"
        raise ArgumentError, 'Evidence path must remain in the case output directory' unless candidate.start_with?(prefix)

        FileUtils.mkdir_p(File.dirname(candidate))
        candidate
      end

      def start
        if @shared_pool
          @shared_pool.start
          prepare_case(@output_directory)
          return browser
        end

        return @browser if @browser
        return @browser = @shared_browser if @shared_browser

        @browser = @browser_factory.start(@browser_name, @configuration)
        prepare_case(@output_directory)
        @browser
      end

      def close_browser
        return @shared_pool.close_browser if @shared_pool

        close
      end

      def new_browser
        if @shared_pool
          @shared_pool.new_browser
          prepare_case(@output_directory)
          return browser
        end

        raise ArgumentError, 'Cannot start a new browser while one is active; close_browser first' if browser

        start
      end

      def reset
        current_browser = browser
        return unless current_browser

        current_browser.cookies.clear if current_browser.respond_to?(:cookies)
        clear_storage
        current_browser.goto('about:blank') if current_browser.respond_to?(:goto)
      end

      def clear_storage
        return unless browser.respond_to?(:execute_script)

        browser.execute_script('window.localStorage.clear(); window.sessionStorage.clear();')
      rescue StandardError
        # about:blank and file pages may deny Web Storage access.
      end

      def prepare_case(output_directory)
        @output_directory = output_directory && File.expand_path(output_directory)
        current_browser = browser
        return unless current_browser && @output_directory
        return unless current_browser.respond_to?(:driver) && current_browser.driver.respond_to?(:execute_cdp)

        current_browser.driver.execute_cdp(
          'Browser.setDownloadBehavior', behavior: 'allow', downloadPath: @output_directory
        )
      end

      def close
        return if @shared_pool
        return unless @browser

        if @owns_browser && @browser.respond_to?(:quit)
          @browser.quit
        elsif @owns_browser && @browser.respond_to?(:close)
          @browser.close
        end
      ensure
        @browser = nil
      end
    end
  end
end
