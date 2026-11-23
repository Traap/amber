# frozen_string_literal: true

# rubocop:disable Metrics/BlockLength -- covers the compact navigation contract
RSpec.describe Amber::Execution::Navigation do
  it 'validates and selects a route' do
    navigation = described_class.new(
      'navigation' => {
        'version' => 1,
        'routes' => [{ 'from' => 'start', 'to' => 'page', 'steps' => [{ 'action' => 'navigate' }] }]
      }
    )

    expect(navigation.route(from: 'start', to: 'page').steps.first['action']).to eq('navigate')
  end
  it 'rejects unsupported versions and empty routes' do
    expect do
      described_class.new('navigation' => { 'version' => 2, 'routes' => [] })
    end.to raise_error(ArgumentError, /version must be 1/)
  end

  it 'passes the selected route and parsed input to an injected adapter' do
    route = Amber::Execution::Navigation::Route.new(from: 'start', to: 'page', steps: [])
    adapter = proc do |selected, input, _context|
      expect(selected.from).to eq(route.from)
      expect(input).to eq('query' => 'Amber')
      Amber::Execution::Result.new(status: :passed)
    end
    runner = Amber::Execution::NavigationRunner.new(
      navigation: instance_double(described_class, route: route),
      action_adapter: instance_double('action adapter'),
      navigation_adapter: adapter
    )
    step = Struct.new(:target, :parameters).new('page', { 'from' => 'start', 'input' => 'query.yaml' })
    context = instance_double('context', navigation_input: { 'query' => 'Amber' })

    expect(runner.execute(step, context)).to be_passed
  end
end
# rubocop:enable Metrics/BlockLength
