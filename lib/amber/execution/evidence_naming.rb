# frozen_string_literal: true

module Amber
  module Execution
    # Generates evidence names consumed by Autodoc's test-evidence macros.
    module EvidenceNaming
      def default_evidence_path(session, step, extension)
        case_name = File.basename(step.filename.to_s, '.*')
        step_number = step.number.to_i.to_s.rjust(3, '0')
        file_number = next_file_number(session, case_name, step_number, extension)
        session.evidence_path("#{case_name}-#{step_number}-#{file_number}.#{extension}")
      end

      private

      def next_file_number(session, case_name, step_number, extension)
        root = session.output_directory || Dir.pwd
        pattern = File.join(root, "#{case_name}-#{step_number}-*.#{extension}")
        numbers = Dir.glob(pattern).filter_map do |path|
          File.basename(path)[/#{case_name}-#{step_number}-(\d+)\.#{extension}\z/, 1]&.to_i
        end
        (numbers.max || 0).next.to_s.rjust(3, '0')
      end
    end
  end
end
