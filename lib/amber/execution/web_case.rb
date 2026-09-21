# frozen_string_literal: true

module Amber
  module Execution
    # Immutable, application-neutral representation of a YAML web case.
    class WebCase
      attr_reader :browser, :configuration, :steps, :navigation_file, :input_root

      def self.from_yaml(data)
        web = data.fetch('web')
        new(
          browser: web.fetch('browser'),
          configuration: web.fetch('configuration', {}),
          steps: data.fetch('steps'),
          navigation_file: web['navigation_file'],
          input_root: web.fetch('input_root', 'config/input')
        )
      end

      def initialize(browser:, steps:, configuration: {}, navigation_file: nil,
                     input_root: 'config/input')
        @browser = browser.to_s
        @configuration = configuration.transform_keys(&:to_sym).freeze
        @steps = steps.map(&:dup).freeze
        @navigation_file = navigation_file
        @input_root = input_root
        validate!
      end

      private

      def validate!
        raise ArgumentError, 'Web case browser must not be empty' if @browser.empty?
        raise ArgumentError, 'Web case configuration must be a mapping' unless @configuration.is_a?(Hash)
        raise ArgumentError, 'Web case steps must be an array' unless @steps.is_a?(Array)

        validate_relative_path(@navigation_file) if @navigation_file
        validate_relative_path(@input_root)
      end

      def validate_relative_path(path)
        raise ArgumentError, 'Web case factory paths must be relative' if Pathname.new(path.to_s).absolute?
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
        steps.map do |step|
          if block_given?
            yield(step)
          else
            adapter.execute(step, context)
          end
        end
      ensure
        adapter.close(session)
      end
    end
  end
end
