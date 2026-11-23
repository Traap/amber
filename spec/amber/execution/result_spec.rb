# frozen_string_literal: true

RSpec.describe Amber::Execution::Result do
  it 'normalizes output and exposes status predicates' do
    result = described_class.new(
      status: :passed,
      stdout: nil,
      evidence: 'screenshot.png',
      metadata: { browser: 'Chrome' }
    )

    expect(result).to be_passed
    expect(result.stdout).to eq('')
    expect(result.evidence).to eq(['screenshot.png'])
    expect(result.metadata).to eq(browser: 'Chrome')
  end

  it 'rejects unsupported statuses' do
    expect { described_class.new(status: :unknown) }
      .to raise_error(ArgumentError, /Unsupported execution status/)
  end
end
