# frozen_string_literal: true

RSpec.describe 'Amber CLI report directory' do
  it 'accepts an explicit report directory' do
    options = Amber::CommandLineOptions.parse(['--report-dir', 'paperboy/report'])

    expect(options.report_dir).to eq(File.expand_path('paperboy/report'))
  end
end
