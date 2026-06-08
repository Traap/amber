# frozen_string_literal: true

require 'amber'
require 'tempfile'

describe Amber::Workflow do
  let(:options) { Amber::Options.new }
  let(:workflow) { described_class.new(options) }

  it 'rejects empty YAML files' do
    Tempfile.create(['empty', '.yaml']) do |file|
      expect { workflow.parse_yaml_file(file.path) }
        .to raise_error(ArgumentError, /must contain a YAML mapping/)
    end
  end

  it 'rejects unsupported top-level YAML keys' do
    Tempfile.create(['unsupported', '.yaml']) do |file|
      file.write("unknown:\n  name: invalid\n")
      file.flush

      expect { workflow.parse_yaml_file(file.path) }
        .to raise_error(ArgumentError, /unsupported key: unknown/)
    end
  end
end
