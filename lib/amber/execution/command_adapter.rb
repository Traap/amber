# frozen_string_literal: true

require 'amber/tof/testevidence'
require 'amber/execution/result'

module Amber
  module Execution
    # Executes the legacy shell-command step through the neutral adapter API.
    # rubocop:disable Metrics/MethodLength -- preserves the adapter lifecycle in one method
    class CommandAdapter
      def execute(step, _context = nil)
        return Result.new(status: :skipped) unless step.options.run?

        started_at = Time.now
        stdout, stderr, process_status = Amber::TestEvidence.run_from_temp_directory(
          step.command, step.workingdir
        )
        finished_at = Time.now

        Result.new(
          status: process_status.success? ? :passed : :failed,
          stdout: stdout,
          stderr: stderr,
          started_at: started_at,
          finished_at: finished_at,
          metadata: { process_status: process_status }
        )
      rescue StandardError => e
        Result.new(
          status: :failed,
          error: e,
          started_at: started_at,
          finished_at: Time.now
        )
      end
    end
    # rubocop:enable Metrics/MethodLength
  end
end
