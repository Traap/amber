# frozen_string_literal: true

# rubocop:disable Metrics/BlockLength -- keeps the YAML dispatch fixture readable
RSpec.describe Amber::TestStep do
  let(:options) { web_options }
  let(:step) { described_class.new('factory/case/web/web.yaml', {}, options, web_data, 1, nil) }

  it 'exposes YAML web action data and selects the web adapter' do
    expect(step.adapter_type).to eq('web')
    expect(step.action).to eq('navigate')
    expect(step.target).to eq('fixture://home')
    expect(step.parameters).to eq('wait' => 1)
    expect(step.run_result).to be_passed
  end

  def web_options
    options = Amber::Options.new
    options.adapter_registry = Amber::Execution::AdapterRegistry.new(
      web: Amber::Execution::WebAdapter.new(
        navigate: proc { |_step, _context| Amber::Execution::Result.new(status: :passed) }
      )
    )
    options.data[:dryrun] = false
    options
  end

  def web_data
    {
      'type' => 'web',
      'action' => 'navigate',
      'target' => 'fixture://home',
      'parameters' => { 'wait' => 1 },
      'confirm' => 'home is opened',
      'expectation' => 'home is visible',
      'evidence' => 'screenshot'
    }
  end
end
# rubocop:enable Metrics/BlockLength -- keeps the YAML dispatch fixture readable
