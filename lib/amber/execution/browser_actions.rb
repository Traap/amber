# frozen_string_literal: true

require 'fileutils'
require 'amber/execution/result'

module Amber
  module Execution
    # Generic browser actions usable by YAML web steps.
    class BrowserActions
      def initialize(session)
        @session = session
      end

      def handlers
        {
          navigate: method(:navigate),
          click: method(:click),
          input: method(:input),
          assert: method(:assert),
          screenshot: method(:screenshot)
        }
      end

      def navigate(step, _context)
        browser.goto(step.target)
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

      def assert(step, _context)
        condition = parameter(step, :condition) || 'visible'
        value = parameter(step, :value)
        matches = assertion_result(condition, value, step)
        matches ? passed : failed("Browser assertion failed: #{condition}")
      end

      def screenshot(step, _context)
        path = parameter(step, :path)
        raise ArgumentError, 'Screenshot action requires parameters.path' if path.to_s.empty?

        FileUtils.mkdir_p(File.dirname(path))
        browser.screenshot.save(path)
        @session.evidence.add(:screenshot, path, browser: @session.browser_name)
        passed(evidence: [@session.evidence.items.last])
      end

      private

      def browser
        @session.browser || raise(ArgumentError, 'Web session is not started')
      end

      def element(step)
        locator = parameter(step, :locator) || step.target
        raise ArgumentError, 'Browser action requires a target or locator' if locator.to_s.empty?

        locator.is_a?(Hash) ? browser.element(**symbolize(locator)) : browser.element(id: locator)
      end

      def parameter(step, name)
        step.parameters[name.to_s] || step.parameters[name.to_sym]
      end

      def assertion_result(condition, value, step)
        case condition.to_s
        when 'visible' then element(step).present?
        when 'text' then element(step).text == value.to_s
        when 'contains' then element(step).text.include?(value.to_s)
        when 'title' then browser.title == value.to_s
        else raise ArgumentError, "Unsupported browser assertion: #{condition}"
        end
      end

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
