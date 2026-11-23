# frozen_string_literal: true

module Amber
  module Execution
    # Normalized result returned by every execution adapter.
    class Result
      STATUSES = %i[passed failed skipped].freeze

      attr_reader :status, :stdout, :stderr, :error, :evidence,
                  :started_at, :finished_at, :metadata

      def initialize(status:, stdout: '', stderr: '', error: nil, evidence: [],
                     started_at: nil, finished_at: nil, metadata: {})
        raise ArgumentError, "Unsupported execution status: #{status}" unless STATUSES.include?(status)

        @status = status
        @stdout = stdout.to_s
        @stderr = stderr.to_s
        @error = error
        @evidence = Array(evidence).freeze
        @started_at = started_at
        @finished_at = finished_at
        @metadata = metadata.dup.freeze
      end

      def passed?
        @status == :passed
      end

      def failed?
        @status == :failed
      end

      def skipped?
        @status == :skipped
      end

      def success?
        passed?
      end
    end
  end
end
