# frozen_string_literal: true

RSpec.describe Amber::Execution::AdapterRegistry do
  let(:adapter) { instance_double('adapter', execute: nil) }

  it 'registers and fetches adapters by YAML type' do
    registry = described_class.new
    registry.register('web', adapter)

    expect(registry.fetch(:web)).to eq(adapter)
    expect(registry.names).to eq([:web])
  end

  it 'rejects duplicate adapter types' do
    registry = described_class.new(web: adapter)

    expect { registry.register('web', adapter) }
      .to raise_error(ArgumentError, /already registered/)
  end

  it 'rejects adapters without execute' do
    expect { described_class.new(web: Object.new) }
      .to raise_error(ArgumentError, /respond to execute/)
  end

  it 'reports an unknown YAML type clearly' do
    expect { described_class.new.fetch('web') }
      .to raise_error(KeyError, /YAML step type: web/)
  end
end
