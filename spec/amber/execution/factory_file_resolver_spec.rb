# frozen_string_literal: true

require 'tmpdir'

RSpec.describe Amber::Execution::FactoryFileResolver do
  subject(:resolver) { described_class.new(report_dir: report_dir) }

  let(:report_dir) { Dir.mktmpdir('amber-report') }

  after { FileUtils.remove_entry(report_dir) }

  it 'resolves fixture targets below factory/config' do
    path = resolver.resolve_fixture('fixture://web/page_mock.html')

    expect(path).to eq(File.join(report_dir, 'factory/config/web/page_mock.html'))
  end

  it 'rejects absolute and traversal paths' do
    expect { resolver.resolve('/etc/passwd') }.to raise_error(ArgumentError)
    expect { resolver.resolve('../outside.yaml') }.to raise_error(ArgumentError)
  end
end
