# frozen_string_literal: true

RSpec.describe Amber::Execution::WebSession do
  let(:browser) { instance_double('browser', quit: nil) }
  let(:factory) { instance_double('browser factory', start: browser) }

  it 'starts one browser and shares its evidence collector' do
    session = described_class.new(browser_factory: factory, browser: 'Chrome')

    expect(session.start).to eq(browser)
    expect(session.start).to eq(browser)
    expect(factory).to have_received(:start).once
    expect(session.evidence).to be_a(Amber::Execution::EvidenceCollector)
  end

  it 'closes the browser and can be started again' do
    session = described_class.new(browser_factory: factory, browser: 'Firefox')
    session.start

    session.close

    expect(browser).to have_received(:quit).once
    expect(session.browser).to be_nil
  end
end
