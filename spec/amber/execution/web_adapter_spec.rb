# frozen_string_literal: true

RSpec.describe Amber::Execution::WebAdapter do
  let(:result) { Amber::Execution::Result.new(status: :passed) }
  let(:step) { instance_double(Amber::TestStep, action: 'navigate') }

  it 'dispatches a YAML action to an injected handler' do
    handler = proc { |received_step, context| [received_step, context].then { result } }
    adapter = described_class.new(navigate: handler)

    expect(adapter.execute(step, :context)).to equal(result)
  end

  it 'rejects an unknown action' do
    adapter = described_class.new

    expect { adapter.execute(step) }
      .to raise_error(KeyError, /No web action registered: navigate/)
  end

  it 'requires handlers to return a normalized result' do
    adapter = described_class.new(navigate: proc { nil })

    expect { adapter.execute(step) }
      .to raise_error(TypeError, /must return Amber::Execution::Result/)
  end
end
