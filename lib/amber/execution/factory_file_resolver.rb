# frozen_string_literal: true

module Amber
  module Execution
    # Resolves consumer-owned files without allowing paths outside factory.
    class FactoryFileResolver
      def initialize(report_dir: nil)
        @root = File.expand_path(File.join(report_dir || Dir.pwd, 'factory'))
      end

      def resolve(relative_path)
        path = safe_path(relative_path)
        candidate = File.expand_path(path, @root)
        ensure_within_root(candidate)
        candidate
      end

      def resolve_fixture(target)
        value = target.to_s
        prefix = 'fixture://'
        raise ArgumentError, 'Fixture target must use fixture:// scheme' unless value.start_with?(prefix)

        resolve(File.join('config', value.delete_prefix(prefix)))
      end

      private

      def safe_path(path)
        value = path.to_s
        raise ArgumentError, 'Factory path must be relative' if value.empty? || Pathname.new(value).absolute?

        value
      end

      def ensure_within_root(candidate)
        prefix = "#{@root}#{File::SEPARATOR}"
        return candidate if candidate.start_with?(prefix)

        raise ArgumentError, 'Factory path must remain below the report factory'
      end
    end
  end
end
