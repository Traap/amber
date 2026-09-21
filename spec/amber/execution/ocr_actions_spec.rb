# frozen_string_literal: true

# rubocop:disable Metrics/BlockLength -- keeps OCR action fixtures readable
RSpec.describe Amber::Execution::OcrActions do
  let(:session) do
    instance_double(
      Amber::Execution::WebSession,
      browser_name: 'Firefox',
      evidence: Amber::Execution::EvidenceCollector.new
    )
  end
  let(:engine) { instance_double('ocr_engine') }
  let(:actions) { described_class.new(session, engine) }
  let(:step) do
    instance_double(
      Amber::TestStep,
      parameters: { path: '/tmp/translated.png', language: 'fr', value: 'Bonjour' }
    )
  end

  it 'extracts translated text and records OCR evidence' do
    allow(engine).to receive(:extract).with('/tmp/translated.png', language: 'fr').and_return('Bonjour le monde')

    result = actions.ocr(step, nil)

    expect(result).to be_passed
    expect(result.stdout).to eq('Bonjour le monde')
    expect(result.metadata[:language]).to eq('fr')
    expect(session.evidence.items.last[:type]).to eq(:ocr)
  end

  it 'returns a failed result when translated text does not match' do
    allow(engine).to receive(:extract).and_return('Hello world')

    result = actions.ocr(step, nil)

    expect(result).to be_failed
    expect(result.error.message).to match(/OCR assertion failed/)
    expect(session.evidence.items.last[:metadata][:language]).to eq('fr')
  end

  it 'supports exact translation comparison' do
    exact_step = instance_double(
      Amber::TestStep,
      parameters: step.parameters.merge(condition: 'exact')
    )
    allow(engine).to receive(:extract).and_return('Bonjour')

    expect(actions.ocr(exact_step, nil)).to be_passed
  end
end
# rubocop:enable Metrics/BlockLength -- keeps OCR action fixtures readable
