# frozen_string_literal: true

require 'tmpdir'

# rubocop:disable Metrics/BlockLength -- keeps the local web fixture readable
RSpec.describe Amber::TestCase, browser: true do
  it 'executes a YAML-shaped web case against a local fixture' do
    options = Amber::Options.new
    options.data[:dryrun] = false
    options.browser_factory = Amber::Execution::BrowserFactory.new
    fixture = File.expand_path('../../../fixtures/web/home.html', __dir__)

    Dir.mktmpdir('amber-web') do |directory|
      test_case = described_class.new(
        fixture,
        {
          'name' => 'local web fixture',
          'web' => { 'browser' => 'Chrome' },
          'steps' => [
            { 'type' => 'web', 'action' => 'navigate', 'target' => "file://#{fixture}" },
            { 'type' => 'web', 'action' => 'assert', 'target' => 'heading',
              'parameters' => { 'condition' => 'text', 'value' => 'Amber' } },
            { 'type' => 'web', 'action' => 'screenshot',
              'parameters' => { 'path' => File.join(directory, 'home.png') } }
          ]
        },
        options
      )

      results = test_case.run_command

      expect(results).to all(be_passed)
      expect(results.last.evidence.last[:type]).to eq(:screenshot)
      expect(File).to exist(File.join(directory, 'home.png'))
    end
  end

  it 'captures a local download and browser PDF as evidence' do
    options = Amber::Options.new
    options.data[:dryrun] = false
    options.browser_factory = Amber::Execution::BrowserFactory.new
    fixture = File.expand_path('../../../fixtures/web/home.html', __dir__)

    Dir.mktmpdir('amber-web-evidence') do |directory|
      test_case = described_class.new(
        fixture,
        {
          'name' => 'local evidence fixture',
          'web' => { 'browser' => 'Chrome', 'configuration' => { 'download_path' => directory } },
          'steps' => [
            { 'type' => 'web', 'action' => 'navigate', 'target' => "file://#{fixture}" },
            { 'type' => 'web', 'action' => 'download', 'target' => 'download',
              'parameters' => { 'path' => File.join(directory, 'fixture.txt') } },
            { 'type' => 'web', 'action' => 'pdf',
              'parameters' => { 'path' => File.join(directory, 'page.pdf') } }
          ]
        },
        options
      )

      results = test_case.run_command

      expect(results).to all(be_passed)
      expect(results[1].evidence.last[:type]).to eq(:download)
      expect(results[2].evidence.last[:type]).to eq(:pdf)
      expect(File).to exist(File.join(directory, 'fixture.txt'))
      expect(File).to exist(File.join(directory, 'page.pdf'))
    end
  end
end
# rubocop:enable Metrics/BlockLength -- keeps the local web fixture readable
