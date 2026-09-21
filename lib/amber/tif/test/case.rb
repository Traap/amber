# frozen_string_literal: true

require 'amber/tif/test'
require 'amber/tif/test/step'
require 'amber/execution/browser_actions'
require 'amber/execution/browser_factory'
require 'amber/execution/web_case'
require 'amber/execution/web_session'
require 'amber/execution/ocr_actions'
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

    private

    # rubocop:disable Metrics/MethodLength -- keeps web lifecycle orchestration together
    def run_web_case
      definition = Amber::Execution::WebCase.from_yaml(@data)
      steps = web_steps(definition)
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
        configuration: definition.configuration
      )
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

    def web_adapter(session)
      return @options.web_adapter_factory.call(session) if @options.web_adapter_factory

      registry = @options.adapter_registry
      return registry.fetch(:web) if registry&.registered?(:web)

      handlers = Amber::Execution::BrowserActions.new(session).handlers
      handlers.merge!(Amber::Execution::OcrActions.new(session, @options.ocr_engine).handlers) if @options.ocr_engine
      Amber::Execution::WebAdapter.new(handlers)
    end

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
