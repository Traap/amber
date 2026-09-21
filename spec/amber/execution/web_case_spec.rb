# frozen_string_literal: true

RSpec.describe Amber::Execution::WebCase do
  it 'maps the generic web YAML representation' do
    definition = described_class.from_yaml(
      'web' => { 'browser' => 'Firefox', 'configuration' => { 'headless' => true } },
      'steps' => [{ 'type' => 'web', 'action' => 'navigate' }]
    )

    expect(definition.browser).to eq('Firefox')
    expect(definition.configuration).to eq(headless: true)
    expect(definition.steps).to eq([{ 'type' => 'web', 'action' => 'navigate' }])
  end

  it 'requires browser selection and steps' do
    expect do
      described_class.from_yaml('web' => {}, 'steps' => [])
    end.to raise_error(KeyError, /browser/)
  end
end

RSpec.describe Amber::Execution::WebCaseRunner do
  let(:session) { instance_double('session') }
  let(:adapter) { instance_double('adapter') }
  let(:steps) { %w[first second] }

  it 'starts, executes steps in order, and closes the injected session' do
    results = %i[first_result second_result]
    allow(adapter).to receive(:start)
    allow(adapter).to receive(:execute).with('first', nil).and_return(results[0])
    allow(adapter).to receive(:execute).with('second', nil).and_return(results[1])
    allow(adapter).to receive(:close)

    actual = described_class.new(session: session, adapter: adapter).run(steps)

    expect(actual).to eq(results)
    expect(adapter).to have_received(:start).with(session).ordered
    expect(adapter).to have_received(:execute).with('first', nil).ordered
    expect(adapter).to have_received(:execute).with('second', nil).ordered
    expect(adapter).to have_received(:close).with(session).ordered
  end

  it 'closes the session when a step raises' do
    allow(adapter).to receive(:start)
    allow(adapter).to receive(:execute).and_raise(RuntimeError, 'web failure')
    allow(adapter).to receive(:close)

    expect { described_class.new(session: session, adapter: adapter).run(steps) }
      .to raise_error(RuntimeError, 'web failure')
    expect(adapter).to have_received(:close).with(session)
  end
end
