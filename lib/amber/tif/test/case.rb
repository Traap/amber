# frozen_string_literal: true

require 'amber/tif/test'
require 'amber/tif/test/step'
require 'amber/execution/browser_actions'
require 'amber/execution/browser_factory'
require 'amber/execution/web_case'
require 'amber/execution/web_session'
require 'amber/execution/ocr_actions'
require 'amber/execution/factory_file_resolver'
require 'amber/execution/navigation'
require 'amber/tof/writers/writer_factory'

module Amber
  # A test case is a collection of one or more test steps.
  class TestCase < Amber::Test
    def initialize(filename, data, options)
      super('Test Case', filename, data, options)
    end

    def run_command
      return run_web_case if @data.key?('web')

      workingdir = set_working_dir
      nbr = 0
      @data['steps'].each do |s|
        nbr += 1
        step = Amber::WriterFactory.get_test_step(
          @filename, @data, @options, s, nbr, workingdir
        )
        step.process
      end
    end

    # Loads consumer-owned YAML input below the configured factory input root.
    def navigation_input(filename)
      definition = Amber::Execution::WebCase.from_yaml(@data)
      resolver = factory_file_resolver
      path = resolver.resolve(File.join(definition.input_root, filename.to_s))
      YAML.safe_load_file(path) || {}
    end

    private

    # rubocop:disable Metrics/MethodLength -- keeps web lifecycle orchestration together
    def run_web_case
      definition = Amber::Execution::WebCase.from_yaml(@data)
      steps = web_steps(definition)
      validate_web_navigation(definition, steps)
      return steps.map { Amber::Execution::Result.new(status: :skipped) } if simulation?

      session = web_session(definition)
      adapter = web_adapter(session)
      writer_steps = web_writer_steps(definition, adapter)

      runner = Amber::Execution::WebCaseRunner.new(session: session, adapter: adapter)
      runner.run(writer_steps, context: self) do |step|
        step.process
        step.decoratee.last_result
      end
    end
    # rubocop:enable Metrics/MethodLength -- keeps web lifecycle orchestration together

    def simulation?
      @options.simulate? || @options.dryrun?
    end

    def web_session(definition)
      Amber::Execution::WebSession.new(
        browser_factory: @options.browser_factory || Amber::Execution::BrowserFactory.new,
        browser: definition.browser,
        configuration: definition.configuration,
        output_directory: web_output_directory
      )
    end

    def web_output_directory
      filename = File.expand_path(@filename)
      relative = filename.sub("#{Dir.pwd}#{File::SEPARATOR}", '')
      root = Amber::TestEvidence.assemble_test_output_root(@options)
      File.join(root, File.dirname(relative))
    end

    def web_steps(definition)
      definition.steps.each_with_index.map do |step_data, index|
        step = Amber::TestStep.new(
          @filename, @data, @options, step_data, index + 1, set_working_dir
        )
        validate_web_step(step)
        step
      end
    end

    # rubocop:disable Metrics/AbcSize, Metrics/MethodLength -- adapter precedence and setup
    def web_adapter(session)
      return @options.web_adapter_factory.call(session) if @options.web_adapter_factory

      registry = @options.adapter_registry
      return registry.fetch(:web) if registry&.registered?(:web)

      resolver = factory_file_resolver
      handlers = Amber::Execution::BrowserActions.new(session, fixture_resolver: resolver).handlers
      handlers.merge!(Amber::Execution::OcrActions.new(session, @options.ocr_engine).handlers) if @options.ocr_engine
      adapter = Amber::Execution::WebAdapter.new(handlers)
      return adapter unless definition_navigation_file(session)

      navigation = Amber::Execution::Navigation.load(
        resolver.resolve(definition_navigation_file(session))
      )
      adapter.register(
        :teleport,
        Amber::Execution::NavigationRunner.new(
          navigation: navigation,
          action_adapter: adapter,
          navigation_adapter: @options.navigation_adapter
        ).method(:execute)
      )
      adapter
    end
    # rubocop:enable Metrics/AbcSize, Metrics/MethodLength

    def definition_navigation_file(_session)
      Amber::Execution::WebCase.from_yaml(@data).navigation_file
    end

    def factory_file_resolver
      Amber::Execution::FactoryFileResolver.new(report_dir: @options.report_dir)
    end

    # rubocop:disable Metrics/AbcSize, Metrics/MethodLength -- validates one navigation contract
    def validate_web_navigation(definition, steps)
      return unless definition.navigation_file

      resolver = factory_file_resolver
      navigation = Amber::Execution::Navigation.load(resolver.resolve(definition.navigation_file))
      steps.select { |step| step.action.to_s == 'teleport' }.each do |step|
        navigation.route(
          from: step.parameters['from'] || step.parameters[:from],
          to: step.target
        )
        input = step.parameters['input'] || step.parameters[:input]
        resolver.resolve(File.join(definition.input_root, input.to_s)) if input
      end
    end
    # rubocop:enable Metrics/AbcSize, Metrics/MethodLength

    def web_writer_steps(definition, adapter)
      definition.steps.each_with_index.map do |step_data, index|
        step = Amber::WriterFactory.get_test_step(
          @filename, @data, @options, step_data, index + 1, set_working_dir
        )
        step.decoratee.execution_adapter = adapter
        step
      end
    end

    def validate_web_step(step)
      return if step.adapter_type == 'web'

      raise ArgumentError, "Web case step #{step.number} must have type: web"
    end

    def set_working_dir
      workingdir = @data['workingdir']
      workingdir += '/..' if @options.language?
      workingdir += '/..' if @options.browser?
      workingdir
    end
  end
end
