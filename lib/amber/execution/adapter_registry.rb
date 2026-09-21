# frozen_string_literal: true

module Amber
  module Execution
    # Registry for adapters selected by YAML step type.
    class AdapterRegistry
      def initialize(adapters = {})
        @adapters = {}
        adapters.each { |type, adapter| register(type, adapter) }
      end

      def register(type, adapter)
        key = normalize_type(type)
        raise ArgumentError, "Adapter already registered: #{key}" if @adapters.key?(key)
        raise ArgumentError, 'Adapter must respond to execute' unless adapter.respond_to?(:execute)

        @adapters[key] = adapter
      end

      def fetch(type)
        key = normalize_type(type)
        @adapters.fetch(key) do
          raise KeyError, "No adapter registered for YAML step type: #{key}"
        end
      end

      def registered?(type)
        @adapters.key?(normalize_type(type))
      end

      def names
        @adapters.keys.freeze
      end

      private

      def normalize_type(type)
        value = type.to_s.strip
        raise ArgumentError, 'Adapter type must not be empty.' if value.empty?

        value.to_sym
      end
    end
  end
end
