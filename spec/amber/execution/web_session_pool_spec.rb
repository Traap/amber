# frozen_string_literal: true

RSpec.describe Amber::Execution::WebSessionPool do
  let(:cookies) { instance_double('cookies', clear: nil) }
  let(:browser) { instance_double('browser', goto: nil, cookies: cookies, quit: nil) }
  let(:factory) { instance_double('browser factory', start: browser) }
  let(:pool) { described_class.new(browser_factory: factory) }

  it 'reuses one browser and resets state between case sessions' do
    first = pool.session(browser: 'Chrome', output_directory: '/tmp/first-case')
    second = pool.session(browser: 'Chrome', output_directory: '/tmp/second-case')

    expect(first.browser).to eq(browser)
    expect(second.browser).to eq(browser)
    expect(factory).to have_received(:start).once
    expect(browser).to have_received(:goto).with('about:blank').once
    expect(cookies).to have_received(:clear).once
  end

  it 'closes the shared browser when the pool closes' do
    pool.session(browser: 'Chrome')

    pool.close

    expect(browser).to have_received(:quit).once
  end
end
