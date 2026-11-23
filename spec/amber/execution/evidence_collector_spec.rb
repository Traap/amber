# frozen_string_literal: true

RSpec.describe Amber::Execution::EvidenceCollector do
  it 'records typed evidence with metadata' do
    collector = described_class.new

    evidence = collector.add(:screenshot, '/tmp/home.png', browser: 'Chrome')

    expect(evidence).to eq(
      type: :screenshot,
      path: '/tmp/home.png',
      metadata: { browser: 'Chrome' }
    )
    expect(collector.items).to eq([evidence])
  end

  it 'supports OCR, downloads, PDFs, and logs' do
    collector = described_class.new

    %i[ocr download pdf log].each { |type| collector.add(type, "#{type}.txt") }

    expect(collector.items.map { |item| item[:type] }).to eq(%i[ocr download pdf log])
  end

  it 'rejects unknown evidence types' do
    expect { described_class.new.add(:video, 'capture.mp4') }
      .to raise_error(ArgumentError, /Unsupported evidence type/)
  end
end
