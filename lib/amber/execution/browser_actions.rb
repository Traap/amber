# frozen_string_literal: true

require 'fileutils'
require 'base64'
require 'amber/execution/result'
require 'amber/execution/evidence_naming'

module Amber
  module Execution
    # Generic browser actions usable by YAML web steps.
    class BrowserActions
      include EvidenceNaming

      def initialize(session, fixture_resolver: nil)
        @session = session
        @fixture_resolver = fixture_resolver
      end

      # rubocop:disable Metrics/MethodLength -- keeps action registration together
      def handlers
        {
          navigate: method(:navigate),
          click: method(:click),
          input: method(:input),
          fill: method(:fill),
          group: method(:group),
          assert: method(:assert),
          screenshot: method(:screenshot),
          download: method(:download),
          pdf: method(:pdf)
        }
      end
      # rubocop:enable Metrics/MethodLength -- keeps action registration together

      def navigate(step, _context)
        browser.goto(navigation_target(step.target))
        passed
      end

      def click(step, _context)
        element(step).click
        passed
      end

      def input(step, _context)
        element(step).set(parameter(step, :value))
        passed
      end

      def fill(step, _context)
        values = parameter(step, :values)
        raise ArgumentError, 'Browser fill action requires parameters.values' unless values.is_a?(Hash)

        values.each { |target, value| browser.element(id: target).set(value) }
        passed
      end

      # rubocop:disable Metrics/AbcSize, Metrics/MethodLength -- executes a logical step
      def group(step, context)
        actions = parameter(step, :actions)
        raise ArgumentError, 'Grouped web step requires parameters.actions' unless actions.is_a?(Array)

        actions.each do |action|
          nested = ActionStep.new(
            action: action.fetch('action'), target: action['target'],
            parameters: action.fetch('parameters', {})
          )
          result = handlers.fetch(nested.action.to_sym).call(nested, context)
          return result if result.failed?
        end

        return screenshot(step, context) if parameter(step, :record).to_s == 'screenshot'

        passed
      end
      # rubocop:enable Metrics/AbcSize, Metrics/MethodLength -- executes a logical step

      def assert(step, _context)
        condition = parameter(step, :condition) || 'visible'
        value = parameter(step, :value)
        matches = assertion_result(condition, value, step)
        matches ? passed : failed("Browser assertion failed: #{condition}")
      end

      def screenshot(step, _context)
        path = evidence_path(step, 'Screenshot')

        browser.screenshot.save(path)
        record_evidence(:screenshot, path)
      end

      def download(step, _context)
        path = evidence_path(step, 'Download')
        element(step).click
        wait_for_file(path, parameter(step, :timeout) || 10)
        record_evidence(:download, path)
      end

      def pdf(step, _context)
        path = evidence_path(step, 'PDF')
        driver = browser.driver
        raise ArgumentError, 'PDF action requires a browser print driver' unless driver.respond_to?(:print_page)

        File.binwrite(path, Base64.decode64(driver.print_page))
        record_evidence(:pdf, path)
      end

      private

      ActionStep = Struct.new(:action, :target, :parameters, keyword_init: true)

      def browser
        @session.browser || raise(ArgumentError, 'Web session is not started')
      end

      def navigation_target(target)
        return target unless target.to_s.start_with?('fixture://')
        return target unless @fixture_resolver

        "file://#{@fixture_resolver.resolve_fixture(target)}"
      end

      def element(step)
        locator = parameter(step, :locator) || step.target
        raise ArgumentError, 'Browser action requires a target or locator' if locator.to_s.empty?

        locator.is_a?(Hash) ? browser.element(**symbolize(locator)) : browser.element(id: locator)
      end

      def parameter(step, name)
        step.parameters[name.to_s] || step.parameters[name.to_sym]
      end

      def evidence_path(step, label)
        path = parameter(step, :path)
        return default_evidence_path(@session, step, 'png') if path.to_s.empty? && label == 'Screenshot'
        raise ArgumentError, "#{label} action requires parameters.path" if path.to_s.empty?

        @session.evidence_path(path)
      end

      def record_evidence(type, path)
        evidence = @session.evidence.add(type, path, browser: @session.browser_name)
        passed(evidence: [evidence])
      end

      def wait_for_file(path, timeout)
        deadline = Process.clock_gettime(Process::CLOCK_MONOTONIC) + timeout.to_f
        return if File.file?(path)

        sleep 0.1 until File.file?(path) || Process.clock_gettime(Process::CLOCK_MONOTONIC) >= deadline
        return if File.file?(path)

        raise ArgumentError, "Download did not create file: #{path}"
      end

      # rubocop:disable Metrics/AbcSize, Metrics/CyclomaticComplexity, Metrics/MethodLength
      def assertion_result(condition, value, step)
        case condition.to_s
        when 'visible' then element(step).present?
        when 'text' then element(step).text == value.to_s
        when 'contains' then element(step).text.include?(value.to_s)
        when 'title' then browser.title == value.to_s
        when 'value' then element(step).value == value.to_s
        when 'enabled' then element(step).enabled?
        when 'disabled' then !element(step).enabled?
        when 'checked' then element(step).checked?
        when 'unchecked' then !element(step).checked?
        else raise ArgumentError, "Unsupported browser assertion: #{condition}"
        end
      end
      # rubocop:enable Metrics/AbcSize, Metrics/CyclomaticComplexity, Metrics/MethodLength

      def symbolize(hash)
        hash.to_h { |key, value| [key.to_sym, value] }
      end

      def passed(evidence: [])
        Result.new(status: :passed, evidence: Array(evidence))
      end

      def failed(message)
        Result.new(status: :failed, error: StandardError.new(message))
      end
    end
  end
end
