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
        'spec/fixtures/web/home.yaml',
        {
          'name' => 'local web fixture',
          'web' => { 'browser' => 'Chrome' },
          'steps' => [
            { 'type' => 'web', 'action' => 'navigate', 'target' => "file://#{fixture}" },
            { 'type' => 'web', 'action' => 'assert', 'target' => 'heading',
              'parameters' => { 'condition' => 'text', 'value' => 'Amber' } },
            { 'type' => 'web', 'action' => 'screenshot',
              'parameters' => { 'path' => 'home.png' } }
          ]
        },
        options
      )

      results = test_case.run_command

      expect(results).to all(be_passed)
      expect(results.last.evidence.last[:type]).to eq(:screenshot)
      expect(File).to exist('test-output/spec/fixtures/web/home.png')
    end
  end

  it 'captures a local download and browser PDF as evidence' do
    options = Amber::Options.new
    options.data[:dryrun] = false
    options.browser_factory = Amber::Execution::BrowserFactory.new
    fixture = File.expand_path('../../../fixtures/web/home.html', __dir__)

    Dir.mktmpdir('amber-web-evidence') do |directory|
      test_case = described_class.new(
        'spec/fixtures/web/home.yaml',
        {
          'name' => 'local evidence fixture',
          'web' => { 'browser' => 'Chrome' },
          'steps' => [
            { 'type' => 'web', 'action' => 'navigate', 'target' => "file://#{fixture}" },
            { 'type' => 'web', 'action' => 'download', 'target' => 'download',
              'parameters' => { 'path' => 'fixture.txt' } },
            { 'type' => 'web', 'action' => 'pdf',
              'parameters' => { 'path' => 'page.pdf' } }
          ]
        },
        options
      )

      results = test_case.run_command

      expect(results).to all(be_passed)
      expect(results[1].evidence.last[:type]).to eq(:download)
      expect(results[2].evidence.last[:type]).to eq(:pdf)
      expect(File).to exist('test-output/spec/fixtures/web/fixture.txt')
      expect(File).to exist('test-output/spec/fixtures/web/page.pdf')
    end
  end

  it 'validates translated OCR text through an injected engine' do
    options = Amber::Options.new
    options.data[:dryrun] = false
    options.browser_factory = Amber::Execution::BrowserFactory.new
    options.ocr_engine = instance_double('ocr_engine', extract: 'Bonjour fixture')
    fixture = File.expand_path('../../../fixtures/web/home.html', __dir__)

    Dir.mktmpdir('amber-web-ocr') do |directory|
      screenshot = 'ocr.png'
      test_case = described_class.new(
        'spec/fixtures/web/home.yaml',
        {
          'name' => 'local OCR fixture',
          'web' => { 'browser' => 'Chrome' },
          'steps' => [
            { 'type' => 'web', 'action' => 'navigate', 'target' => "file://#{fixture}" },
            { 'type' => 'web', 'action' => 'screenshot', 'parameters' => { 'path' => screenshot } },
            { 'type' => 'web', 'action' => 'ocr',
              'parameters' => {
                'path' => screenshot, 'language' => 'fr', 'value' => 'Bonjour',
                'condition' => 'contains'
              } }
          ]
        },
        options
      )

      results = test_case.run_command

      expect(results).to all(be_passed)
      expect(results.last.stdout).to eq('Bonjour fixture')
      expect(results.last.evidence.last[:type]).to eq(:ocr)
      expect(results.last.evidence.last[:metadata][:language]).to eq('fr')
    end
  end
end
# rubocop:enable Metrics/BlockLength -- keeps the local web fixture readable
