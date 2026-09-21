# frozen_string_literal: true

# rubocop:disable Metrics/BlockLength -- keeps web case fixtures readable
RSpec.describe Amber::TestCase do
  let(:options) { Amber::Options.new }
  let(:browser_factory) { instance_double('browser_factory') }
  let(:adapter) { instance_double('web_adapter') }
  let(:test_case) { described_class.new('fixture/web.yaml', data, options) }
  let(:data) do
    {
      'name' => 'generic web case',
      'web' => { 'browser' => 'Firefox', 'configuration' => { 'headless' => true } },
      'steps' => [{ 'type' => 'web', 'action' => 'navigate', 'target' => 'fixture://home' }]
    }
  end

  it 'runs an explicit web case through injected factories' do
    options.browser_factory = browser_factory
    options.web_adapter_factory = proc { |session| expect(session.browser_name).to eq('Firefox'); adapter }
    allow(adapter).to receive(:start)
    allow(adapter).to receive(:execute).and_return(Amber::Execution::Result.new(status: :passed))
    allow(adapter).to receive(:close)

    results = test_case.run_command

    expect(results).to all(be_passed)
    expect(adapter).to have_received(:start)
    expect(adapter).to have_received(:execute)
    expect(adapter).to have_received(:close)
  end

  it 'rejects command steps in an explicit web case' do
    data['steps'][0]['type'] = 'command'

    expect { test_case.run_command }
      .to raise_error(ArgumentError, /must have type: web/)
  end
end
# rubocop:enable Metrics/BlockLength -- keeps web case fixtures readable
