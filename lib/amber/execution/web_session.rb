# frozen_string_literal: true

require 'fileutils'
require 'amber/execution/evidence_collector'

module Amber
  module Execution
    # Browser lifecycle boundary used by the web adapter.
    class WebSession
      attr_reader :browser, :browser_name, :configuration, :evidence, :output_directory

      def initialize(browser_factory:, browser:, configuration: {}, evidence: nil, output_directory: nil,
                     shared_browser: nil)
        @browser_factory = browser_factory
        @browser_name = browser.to_s
        @shared_browser = shared_browser
        @browser = shared_browser
        @owns_browser = shared_browser.nil?
        @output_directory = output_directory && File.expand_path(output_directory)
        @configuration = configuration.dup
        @configuration[:download_path] ||= @output_directory if @output_directory
        @configuration.freeze
        @evidence = evidence || EvidenceCollector.new
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
        return @browser if @browser
        return @browser = @shared_browser if @shared_browser

        @browser = @browser_factory.start(@browser_name, @configuration)
      end

      def reset
        return unless @browser

        @browser.goto('about:blank') if @browser.respond_to?(:goto)
        @browser.cookies.clear if @browser.respond_to?(:cookies)
        return unless @browser.respond_to?(:execute_script)

        @browser.execute_script('window.localStorage.clear(); window.sessionStorage.clear();')
      end

      def prepare_case(output_directory)
        @output_directory = output_directory && File.expand_path(output_directory)
        return unless @browser && @output_directory
        return unless @browser.respond_to?(:driver) && @browser.driver.respond_to?(:execute_cdp)

        @browser.driver.execute_cdp(
          'Browser.setDownloadBehavior', behavior: 'allow', downloadPath: @output_directory
        )
      end

      def close
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
