# frozen_string_literal: true

require 'amber/execution/result'

module Amber
  module Execution
    # Generic OCR action backed by an injected engine.
    class OcrActions
      def initialize(session, engine)
        @session = session
        @engine = engine
      end

      def handlers
        { ocr: method(:ocr) }
      end

      def ocr(step, _context)
        path = parameter(step, :path)
        raise ArgumentError, 'OCR action requires parameters.path' if path.to_s.empty?

        path = @session.evidence_path(path)

        language = parameter(step, :language)
        text = extract_text(path, language)
        evidence = record_ocr(path, language, text)
        return failed(step, text, evidence) unless matches?(step, text)

        passed(text, language, evidence)
      end

      private

      def parameter(step, name)
        step.parameters[name.to_s] || step.parameters[name.to_sym]
      end

      def extract_text(path, language)
        @engine.extract(path, language: language).to_s
      end

      def record_ocr(path, language, text)
        @session.evidence.add(
          :ocr,
          path,
          browser: @session.browser_name,
          language: language,
          text: text
        )
      end

      def matches?(step, text)
        expected = parameter(step, :value)
        return true if expected.nil?

        case (parameter(step, :condition) || 'contains').to_s
        when 'exact', 'text' then text == expected.to_s
        when 'contains' then text.include?(expected.to_s)
        else raise ArgumentError, "Unsupported OCR assertion: #{parameter(step, :condition)}"
        end
      end

      def failed(step, text, evidence)
        expected = parameter(step, :value)
        Result.new(
          status: :failed,
          stdout: text,
          evidence: [evidence],
          error: StandardError.new("OCR assertion failed: expected #{expected.inspect}")
        )
      end

      def passed(text, language, evidence)
        Result.new(
          status: :passed,
          stdout: text,
          evidence: [evidence],
          metadata: { language: language }
        )
      end
    end
  end
end
