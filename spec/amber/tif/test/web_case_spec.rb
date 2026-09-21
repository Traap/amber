# frozen_string_literal: true

# rubocop:disable Metrics/BlockLength -- keeps web case fixtures readable
RSpec.describe Amber::TestCase do
  let(:options) { Amber::Options.new }
  let(:browser_factory) { instance_double('browser_factory') }
  let(:adapter) { instance_double('web_adapter') }
  let(:test_case) { described_class.new('factory/case/web/generic.yaml', data, options) }
  let(:data) do
    {
      'name' => 'generic web case',
      'web' => { 'browser' => 'Firefox', 'configuration' => { 'headless' => true } },
      'steps' => [{ 'type' => 'web', 'action' => 'navigate', 'target' => 'fixture://home' }]
    }
  end

  before do
    options.data[:dryrun] = false
    allow(browser_factory).to receive(:start).and_return(instance_double('browser'))
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

  it 'adds the injected OCR engine to the default web adapter' do
    data['steps'][0] = {
      'type' => 'web',
      'action' => 'ocr',
      'parameters' => { 'path' => 'page.png', 'language' => 'fr', 'value' => 'Bonjour' }
    }
    ocr_engine = instance_double('ocr_engine', extract: 'Bonjour le monde')
    browser = instance_double('browser')
    allow(browser_factory).to receive(:start).and_return(browser)
    options.browser_factory = browser_factory
    options.ocr_engine = ocr_engine

    results = test_case.run_command

    expect(results).to all(be_passed)
    expect(ocr_engine).to have_received(:extract).with(
      File.expand_path(
        File.join(Amber::TestEvidence::TEST_OUTPUT_DIR, 'factory/case/web/page.png')
      ), language: 'fr'
    )
  end

  it 'resolves case-local input by target id before execution' do
    data['web']['input'] = { 'start-date' => '2026-09-20' }
    data['steps'][0] = {
      'type' => 'web', 'action' => 'input', 'target' => 'start-date'
    }
    options.browser_factory = browser_factory
    options.web_adapter_factory = proc { adapter }
    allow(adapter).to receive(:start)
    allow(adapter).to receive(:execute).and_return(Amber::Execution::Result.new(status: :passed))
    allow(adapter).to receive(:close)

    test_case.run_command

    expect(adapter).to have_received(:execute).with(
      an_object_having_attributes(parameters: { 'value' => '2026-09-20' }), anything
    )
  end

  it 'validates web steps but does not start a browser during simulation' do
    options.data[:simulate] = true
    options.browser_factory = browser_factory
    options.web_adapter_factory = proc { raise 'adapter must not be created' }

    results = test_case.run_command

    expect(results).to all(be_skipped)
    expect(browser_factory).not_to have_received(:start)
  end
end
# rubocop:enable Metrics/BlockLength -- keeps web case fixtures readable
