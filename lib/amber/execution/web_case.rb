# frozen_string_literal: true

module Amber
  module Execution
    # Immutable, application-neutral representation of a YAML web case.
    class WebCase
      attr_reader :browser, :configuration, :steps

      def self.from_yaml(data)
        web = data.fetch('web')
        new(
          browser: web.fetch('browser'),
          configuration: web.fetch('configuration', {}),
          steps: data.fetch('steps')
        )
      end

      def initialize(browser:, steps:, configuration: {})
        @browser = browser.to_s
        @configuration = configuration.transform_keys(&:to_sym).freeze
        @steps = steps.map(&:dup).freeze
        validate!
      end

      private

      def validate!
        raise ArgumentError, 'Web case browser must not be empty' if @browser.empty?
        raise ArgumentError, 'Web case configuration must be a mapping' unless @configuration.is_a?(Hash)
        raise ArgumentError, 'Web case steps must be an array' unless @steps.is_a?(Array)
      end
    end

    # Executes a web case using only injected session, adapter, and steps.
    class WebCaseRunner
      attr_reader :session, :adapter

      def initialize(session:, adapter:)
        @session = session
        @adapter = adapter
      end

      def run(steps, context: nil)
        adapter.start(session)
        steps.map { |step| adapter.execute(step, context) }
      ensure
        adapter.close(session)
      end
    end
  end
end
