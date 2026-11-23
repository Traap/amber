# frozen_string_literal: true

require 'fileutils'
require 'tmpdir'

rspec_output_dir = ENV['AMBER_TEST_OUTPUT_DIR']
unless rspec_output_dir
  rspec_output_dir = Dir.mktmpdir('amber-rspec-test-output')
  ENV['AMBER_TEST_OUTPUT_DIR'] = rspec_output_dir
  at_exit { FileUtils.remove_entry(rspec_output_dir) }
end

require 'amber'

# Test Coverage
require 'simplecov'
SimpleCov.command_name 'Unit Tests'
SimpleCov.start

RSpec.configure do |config|
  config.filter_run_excluding(browser: true) unless ENV['AMBER_BROWSER_SPECS'] == '1'

  config.expect_with :rspec do |expectations|
    expectations.include_chain_clauses_in_custom_matcher_descriptions = true
  end

  config.mock_with :rspec do |mocks|
    mocks.verify_partial_doubles = true
  end

  config.shared_context_metadata_behavior = :apply_to_host_groups
end
