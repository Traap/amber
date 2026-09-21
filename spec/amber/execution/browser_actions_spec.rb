# frozen_string_literal: true

# rubocop:disable Metrics/BlockLength -- keeps browser action fixtures readable
RSpec.describe Amber::Execution::BrowserActions do
  let(:browser) { instance_double('browser') }
  let(:session) do
    instance_double(
      Amber::Execution::WebSession,
      browser: browser,
      browser_name: 'Chrome',
      evidence: Amber::Execution::EvidenceCollector.new
    )
  end
  let(:actions) { described_class.new(session) }

  before do
    allow(session).to receive(:evidence_path) { |path| path }
  end

  def step(action, target: 'heading', parameters: {})
    instance_double(
      Amber::TestStep,
      action: action,
      target: target,
      parameters: parameters
    )
  end

  it 'navigates to a target' do
    allow(browser).to receive(:goto)

    result = actions.navigate(step('navigate', target: 'fixture://home'), nil)

    expect(browser).to have_received(:goto).with('fixture://home')
    expect(result).to be_passed
  end

  it 'clicks a target element by id' do
    element = instance_double('element', click: nil)
    allow(browser).to receive(:element).with(id: 'submit').and_return(element)

    result = actions.click(step('click', target: 'submit'), nil)

    expect(element).to have_received(:click)
    expect(result).to be_passed
  end

  it 'asserts text and reports a failed result' do
    element = instance_double('element', text: 'Wrong text')
    allow(browser).to receive(:element).with(id: 'heading').and_return(element)

    result = actions.assert(step('assert', parameters: { condition: 'text', value: 'Expected' }), nil)

    expect(result).to be_failed
    expect(result.error.message).to match(/Browser assertion failed/)
  end

  it 'fills multiple controls by id' do
    first = instance_double('element', set: nil)
    second = instance_double('element', set: nil)
    allow(browser).to receive(:element).with(id: 'start-date').and_return(first)
    allow(browser).to receive(:element).with(id: 'end-date').and_return(second)

    values = { 'start-date' => '2026-07-01', 'end-date' => '2026-07-31' }
    fill_step = step('fill', target: 'date-range-form', parameters: { values: values })
    result = actions.fill(fill_step, nil)

    expect(first).to have_received(:set).with('2026-07-01')
    expect(second).to have_received(:set).with('2026-07-31')
    expect(result).to be_passed
  end

  it 'executes grouped actions and records one screenshot' do
    first = instance_double('element', set: nil)
    submit = instance_double('element', enabled?: true)
    screenshot = instance_double('screenshot', save: nil)
    allow(browser).to receive(:element).with(id: 'start-date').and_return(first)
    allow(browser).to receive(:element).with(id: 'date-submit').and_return(submit)
    allow(browser).to receive(:screenshot).and_return(screenshot)

    grouped = step(
      'group',
      parameters: {
        actions: [
          { 'action' => 'fill', 'parameters' => { 'values' => { 'start-date' => '2026-09-20' } } },
          { 'action' => 'assert', 'target' => 'date-submit',
            'parameters' => { 'condition' => 'enabled' } }
        ],
        record: 'screenshot',
        path: '/tmp/amber-grouped.png'
      }
    )

    result = actions.group(grouped, nil)

    expect(first).to have_received(:set).with('2026-09-20')
    expect(submit).to have_received(:enabled?)
    expect(screenshot).to have_received(:save)
    expect(result).to be_passed
    expect(session.evidence.items.last[:type]).to eq(:screenshot)
  end

  it 'asserts an enabled control' do
    element = instance_double('element', enabled?: true)
    allow(browser).to receive(:element).with(id: 'submit').and_return(element)

    result = actions.assert(
      step('assert', target: 'submit', parameters: { condition: 'enabled' }), nil
    )

    expect(result).to be_passed
  end

  it 'asserts a selected control value' do
    element = instance_double('element', value: '5')
    allow(browser).to receive(:element).with(id: 'integer').and_return(element)

    result = actions.assert(
      step('assert', target: 'integer', parameters: { condition: 'value', value: '5' }), nil
    )

    expect(result).to be_passed
  end

  it 'asserts checked and unchecked controls' do
    checked = instance_double('element', checked?: true)
    unchecked = instance_double('element', checked?: false)
    allow(browser).to receive(:element).with(id: 'choice-a').and_return(checked)
    allow(browser).to receive(:element).with(id: 'choice-b').and_return(unchecked)

    checked_result = actions.assert(
      step('assert', target: 'choice-a', parameters: { condition: 'checked' }), nil
    )
    unchecked_result = actions.assert(
      step('assert', target: 'choice-b', parameters: { condition: 'unchecked' }), nil
    )

    expect(checked_result).to be_passed
    expect(unchecked_result).to be_passed
  end

  it 'captures a screenshot as evidence' do
    screenshot = instance_double('screenshot', save: nil)
    allow(browser).to receive(:screenshot).and_return(screenshot)

    result = actions.screenshot(
      step('screenshot', parameters: { path: '/tmp/amber-browser.png' }),
      nil
    )

    expect(screenshot).to have_received(:save).with('/tmp/amber-browser.png')
    expect(result).to be_passed
    expect(session.evidence.items.first[:type]).to eq(:screenshot)
  end

  it 'captures a downloaded file as evidence' do
    path = '/tmp/amber-download.txt'
    element = instance_double('element', click: nil)
    allow(browser).to receive(:element).with(id: 'download').and_return(element)
    allow(File).to receive(:file?).with(path).and_return(true)

    result = actions.download(step('download', target: 'download', parameters: { path: path }), nil)

    expect(element).to have_received(:click)
    expect(result).to be_passed
    expect(session.evidence.items.last[:type]).to eq(:download)
  end

  it 'captures a browser PDF as evidence' do
    driver = instance_double('driver', print_page: Base64.strict_encode64('%PDF-fixture'))
    allow(browser).to receive(:driver).and_return(driver)
    path = '/tmp/amber-page.pdf'

    result = actions.pdf(step('pdf', parameters: { path: path }), nil)

    expect(File.binread(path)).to eq('%PDF-fixture')
    expect(result).to be_passed
    expect(session.evidence.items.last[:type]).to eq(:pdf)
  end
end
# rubocop:enable Metrics/BlockLength
