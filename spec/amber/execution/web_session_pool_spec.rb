# frozen_string_literal: true

# rubocop:disable Metrics/BlockLength -- lifecycle examples cover the complete pool contract
RSpec.describe Amber::Execution::WebSessionPool do
  let(:cookies) { instance_double('cookies', clear: nil) }
  let(:browser) { instance_double('browser', goto: nil, cookies: cookies, quit: nil) }
  let(:factory) { instance_double('browser factory', start: browser) }
  let(:pool) { described_class.new(browser_factory: factory) }

  it 'starts lazily and reuses one browser between case sessions' do
    first = pool.session(browser: 'Chrome', output_directory: '/tmp/first-case')
    expect(factory).not_to have_received(:start)

    first.start
    second = pool.session(browser: 'Chrome', output_directory: '/tmp/second-case')

    expect(first.browser).to eq(browser)
    expect(second.browser).to eq(browser)
    expect(factory).to have_received(:start).once
    expect(browser).to have_received(:goto).with('about:blank').once
    expect(cookies).to have_received(:clear).once
  end

  it 'starts a replacement browser only after the current one is closed' do
    session = pool.session(browser: 'Chrome')
    session.start

    expect { session.new_browser }
      .to raise_error(ArgumentError, /close_browser first/)

    session.close_browser
    session.new_browser

    expect(factory).to have_received(:start).twice
    expect(browser).to have_received(:quit).once
  end

  it 'allows closing an inactive browser' do
    session = pool.session(browser: 'Chrome')

    expect { session.close_browser }.not_to raise_error
    expect(factory).not_to have_received(:start)
  end

  it 'closes the shared browser when the pool closes' do
    pool.session(browser: 'Chrome').start

    pool.close

    expect(browser).to have_received(:quit).once
  end
end
# rubocop:enable Metrics/BlockLength
