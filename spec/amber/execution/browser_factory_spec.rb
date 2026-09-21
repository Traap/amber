# frozen_string_literal: true

RSpec.describe Amber::Execution::BrowserFactory do
  subject(:factory) { described_class.new }

  it 'supports the required browser matrix' do
    expect(described_class::SUPPORTED_BROWSERS).to eq(%w[Brave Chrome Edge Firefox])
  end

  it 'rejects unsupported browsers' do
    expect { factory.start('Opera') }
      .to raise_error(ArgumentError, /Unsupported web browser: Opera/)
  end

  it 'maps Brave to the Chrome WebDriver implementation' do
    expect(factory.send(:watir_browser, 'Brave')).to eq(:chrome)
  end

  it 'uses configured driver environment keys' do
    expect(described_class::DRIVER_ENV).to eq(
      'Chrome' => 'AMBER_CHROMEDRIVER_PATH',
      'Brave' => 'AMBER_BRAVEDRIVER_PATH',
      'Edge' => 'AMBER_EDGEDRIVER_PATH',
      'Firefox' => 'AMBER_GECKODRIVER_PATH'
    )
  end
end
