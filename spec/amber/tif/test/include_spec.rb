# frozen_string_literal: true

# {{{ Required files.

require 'amber'

# -------------------------------------------------------------------------- }}}
def include_file
  Amber::Include.new({}, Amber::Options.new)
end

describe 'Test Input Factory Include files' do
  it 'maps simple include arrays to amber command options' do
    names = [{ 'name' => 'advanced-concept' }]

    expect(include_file.send(:map_to_files, names, '--suite')).to eq('--suite=advanced-concept')
  end

  it 'maps multiple simple include entries to comma-separated names' do
    names = [{ 'name' => 'browser' }, { 'name' => 'case' }]

    expect(include_file.send(:map_to_files, names, '--case')).to eq('--case=browser,case')
  end
end

describe 'Test Input Factory nested Include files' do
  it 'maps nested include arrays to folder-qualified command options' do
    nested_include_file = include_file
    nested_include_file.instance_variable_set(:@folder, 'cli')
    names = [{ 'name' => 'options' }]

    expect(nested_include_file.send(:map_to_nested_files, names, '--suite')).to eq('--suite=cli/options')
  end

  it 'maps multiple nested include entries to comma-separated folder-qualified names' do
    nested_include_file = include_file
    nested_include_file.instance_variable_set(:@folder, 'cli/options')
    names = [{ 'name' => 'browser' }, { 'name' => 'case' }]

    expected = '--case=cli/options/browser,cli/options/case'
    expect(nested_include_file.send(:map_to_nested_files, names, '--case')).to eq(expected)
  end
end

describe 'Test Input Factory Include command' do
  around do |example|
    original_amberpath = ENV.fetch('AMBERPATH', nil)
    ENV['AMBERPATH'] = '/home/example/amber'
    example.run
    ENV['AMBERPATH'] = original_amberpath
  end

  it 'uses the amber executable from AMBERPATH' do
    command = include_file.send(:assemble_amber_command, '--suite=cli/options')

    expect(command).to start_with('/home/example/amber/bin/amber ')
    expect(command).to include('--suite=cli/options')
  end
end

# -------------------------------------------------------------------------- }}}
