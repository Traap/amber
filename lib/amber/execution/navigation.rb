# frozen_string_literal: true

require 'amber/execution/result'

require 'yaml'

module Amber
  module Execution
    # Loads and validates the neutral YAML navigation contract.
    class Navigation
      SUPPORTED_VERSION = 1
      Route = Struct.new(:from, :to, :steps, keyword_init: true)

      attr_reader :routes

      def self.load(path)
        new(YAML.safe_load_file(path), path)
      end

      def initialize(data, path = nil)
        @path = path
        @routes = parse(data)
      end

      def route(from:, to:)
        @routes.fetch([from.to_s, to.to_s]) do
          raise ArgumentError, "Navigation route not found: #{from} -> #{to}"
        end
      end

      private

      # rubocop:disable Metrics/AbcSize, Metrics/CyclomaticComplexity, Metrics/MethodLength, Metrics/PerceivedComplexity
      def parse(data)
        navigation = data.is_a?(Hash) && data['navigation']
        invalid('top-level key must be navigation') unless navigation.is_a?(Hash)
        invalid('navigation.version must be 1') unless navigation['version'] == SUPPORTED_VERSION

        routes = navigation['routes']
        invalid('navigation.routes must be an array') unless routes.is_a?(Array)
        invalid('navigation.routes must not be empty') if routes.empty?

        routes.to_h do |route|
          invalid('each route must be a mapping') unless route.is_a?(Hash)
          from = required_string(route, 'from')
          to = required_string(route, 'to')
          steps = route['steps']
          invalid("route #{from} -> #{to} requires steps") unless steps.is_a?(Array) && !steps.empty?
          steps = steps.map { |step| validate_step(step, from, to) }
          [[from, to], Route.new(from: from, to: to, steps: steps.freeze)]
        end.freeze
      rescue Psych::Exception => e
        raise ArgumentError, "Invalid navigation YAML#{@path && " in #{@path}"}: #{e.message}"
      end
      # rubocop:enable Metrics/AbcSize, Metrics/CyclomaticComplexity, Metrics/MethodLength, Metrics/PerceivedComplexity

      def validate_step(step, from, to)
        invalid("route #{from} -> #{to} contains a malformed step") unless step.is_a?(Hash)
        action = required_string(step, 'action')
        step.merge('action' => action).freeze
      end

      def required_string(mapping, key)
        value = mapping[key]
        invalid("#{key} must be a non-empty string") unless value.is_a?(String) && !value.strip.empty?
        value
      end

      def invalid(message)
        raise ArgumentError, "Invalid navigation#{@path && " in #{@path}"}: #{message}"
      end
    end

    # Executes a teleport route using either an injected adapter or web actions.
    class NavigationRunner
      Step = Struct.new(:action, :target, :parameters, keyword_init: true)

      def initialize(navigation:, action_adapter:, navigation_adapter: nil)
        @navigation = navigation
        @action_adapter = action_adapter
        @navigation_adapter = navigation_adapter
      end

      # rubocop:disable Metrics/AbcSize, Metrics/MethodLength -- keeps route execution together
      def execute(step, context)
        route = @navigation.route(
          from: parameter(step, :from),
          to: step.target
        )
        input = context.navigation_input(parameter(step, :input)) if parameter(step, :input)
        return injected(route, input, context) if @navigation_adapter

        route.steps.map do |route_step|
          action_step = Step.new(
            action: route_step['action'], target: route_step['target'],
            parameters: route_step['parameters'] || {}
          )
          @action_adapter.execute(action_step, context)
        end.reverse.find(&:failed?) || Result.new(status: :passed)
      end
      # rubocop:enable Metrics/AbcSize, Metrics/MethodLength

      private

      def parameter(step, name)
        step.parameters[name.to_s] || step.parameters[name.to_sym]
      end

      def injected(route, input, context)
        result = @navigation_adapter.call(route, input, context)
        raise TypeError, 'Navigation adapter must return Amber::Execution::Result' unless result.is_a?(Result)

        result
      end
    end
  end
end
