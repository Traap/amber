# frozen_string_literal: true

require 'amber/execution/result'

module Amber
  module Execution
    # Dispatches generic YAML web actions to injected handlers.
    class WebAdapter
      def initialize(actions = {})
        @actions = {}
        actions.each { |action, handler| register(action, handler) }
      end

      def register(action, handler)
        key = normalize_action(action)
        raise ArgumentError, "Web action already registered: #{key}" if @actions.key?(key)
        raise ArgumentError, "Web action must be callable: #{key}" unless handler.respond_to?(:call)

        @actions[key] = handler
      end

      def start(session)
        session.start
      end

      def close(session)
        session.close
      end

      def execute(step, context = nil)
        handler = @actions.fetch(normalize_action(step.action)) do
          raise KeyError, "No web action registered: #{step.action}"
        end
        result = handler.call(step, context)
        raise TypeError, 'Web action must return Amber::Execution::Result' unless result.is_a?(Result)

        result
      rescue KeyError, TypeError
        raise
      rescue StandardError => e
        Result.new(status: :failed, error: e)
      end

      private

      def normalize_action(action)
        value = action.to_s.strip
        raise ArgumentError, 'Web action must not be empty.' if value.empty?

        value.to_sym
      end
    end
  end
end
