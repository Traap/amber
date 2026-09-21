# frozen_string_literal: true

require 'fileutils'

module Amber
  module Execution
    # Creates browser sessions without exposing Watir/Selenium to Amber's core.
    class BrowserFactory
      SUPPORTED_BROWSERS = %w[Brave Chrome Edge Firefox].freeze
      DRIVER_ENV = {
        'Chrome' => 'AMBER_CHROMEDRIVER_PATH',
        'Brave' => 'AMBER_BRAVEDRIVER_PATH',
        'Edge' => 'AMBER_EDGEDRIVER_PATH',
        'Firefox' => 'AMBER_GECKODRIVER_PATH'
      }.freeze
      DRIVER_COMMAND = {
        'Chrome' => 'chromedriver',
        'Brave' => 'chromedriver',
        'Edge' => 'msedgedriver',
        'Firefox' => 'geckodriver'
      }.freeze

      def start(browser, configuration = {})
        browser_name = normalize_browser(browser)
        require 'watir'
        download_path = configuration[:download_path] || default_download_path
        FileUtils.mkdir_p(download_path)
        configure_driver(browser_name, configuration[:driver_path])

        Watir::Browser.new(
          watir_browser(browser_name),
          options: browser_options(browser_name, configuration, download_path)
        )
      end

      private

      def normalize_browser(browser)
        value = browser.to_s
        return value if SUPPORTED_BROWSERS.include?(value)

        raise ArgumentError, "Unsupported web browser: #{browser}"
      end

      def watir_browser(browser)
        browser == 'Brave' ? :chrome : browser.downcase.to_sym
      end

      def configure_driver(browser, configured_path)
        path = configured_path || ENV[DRIVER_ENV.fetch(browser)] ||
               executable_path(DRIVER_COMMAND.fetch(browser))
        return if path.to_s.empty?

        driver_service(browser).driver_path = path
      end

      def executable_path(command)
        ENV.fetch('PATH', '').split(File::PATH_SEPARATOR).map do |directory|
          path = File.join(directory, command)
          return path if File.file?(path) && File.executable?(path)
        end
        nil
      end

      def driver_service(browser)
        case browser
        when 'Chrome', 'Brave'
          Selenium::WebDriver::Chrome::Service
        when 'Edge'
          Selenium::WebDriver::Edge::Service
        when 'Firefox'
          Selenium::WebDriver::Firefox::Service
        end
      end

      def browser_options(browser, configuration, download_path)
        preferences = {
          'download.default_directory' => download_path,
          'download.prompt_for_download' => false
        }
        preferences['plugins.always_open_pdf_externally'] = true

        options = { prefs: preferences }
        binary = configuration[:binary] || browser_binary(browser)
        options[:binary] = binary unless binary.to_s.empty?
        options
      end

      def browser_binary(browser)
        if browser == 'Brave'
          return ENV['AMBER_BRAVE_BINARY'] unless ENV['AMBER_BRAVE_BINARY'].to_s.empty?
          return '/opt/brave-bin/brave' if File.executable?('/opt/brave-bin/brave')
        end

        if browser == 'Edge'
          return ENV['AMBER_EDGE_BINARY'] unless ENV['AMBER_EDGE_BINARY'].to_s.empty?
          return '/opt/microsoft/msedge/microsoft-edge' if
            File.executable?('/opt/microsoft/msedge/microsoft-edge')
        end

        ''
      end

      def default_download_path
        ENV.fetch('AMBER_DOWNLOAD_PATH', File.join(Dir.home, 'Downloads'))
      end
    end
  end
end
