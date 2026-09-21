# frozen_string_literal: true

module Amber
  module Execution
    # Collects evidence attachments independently of any report writer.
    class EvidenceCollector
      TYPES = %i[screenshot download pdf ocr log].freeze

      attr_reader :items

      def initialize
        @items = []
      end

      def add(type, path, metadata = {})
        kind = normalize_type(type)
        @items << {
          type: kind,
          path: path.to_s,
          metadata: metadata.dup.freeze
        }.freeze
        @items.last
      end

      def empty?
        @items.empty?
      end

      private

      def normalize_type(type)
        value = type.to_s.strip.downcase.to_sym
        raise ArgumentError, "Unsupported evidence type: #{type}" unless TYPES.include?(value)

        value
      end
    end
  end
end
