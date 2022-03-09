# frozen_string_literal: true

require 'open3'
require 'bundler/gem_tasks'
require 'rspec/core/rake_task'

# ------------------------------------------------------------------------------
# Prerequisite checks.
# ------------------------------------------------------------------------------

if ENV['AMBERPATH'].nil?
  puts 'WARNING: Amber is not installed.'
  abort = true
end

if ENV['DOCBLDPATH'].nil?
  puts 'WARNING: docbld is not installed.'
  abort = true
end

exit if abort

# ------------------------------------------------------------------------------
# Run rspecs.
# ------------------------------------------------------------------------------

begin
  RSpec::Core::RakeTask.new(:spec)
  task default: :spec
rescue StandardError
  puts 'RSpec is not supported on this system.'
end

# ------------------------------------------------------------------------------
# Build Amber.
# ------------------------------------------------------------------------------

namespace :build do
  task :amber do
    system 'bundle install'
    system 'bundle exec rake'
    system 'bundle exec rake install'
  end
end

# ------------------------------------------------------------------------------
# Validate Amber.
# ------------------------------------------------------------------------------

namespace :validate do
  report_dir = "#{ENV['AMBERPATH']}/report"
  validate_cmd = 'amber --nodryrun --log-environment --obliterate --plan=master'
  pdf_cmd = "rake --rakefile #{ENV['DOCBLDPATH']}/Rakefile"
  pwd = ''

  task amber: %i[save_wd report_dir do_validation restore_wd docbld]

  task iotest: %i[save_wd report_dir do_validation restore_wd]

  task :save_wd do
    pwd = Dir.getwd
  end

  task :report_dir do
    Dir.chdir report_dir
  end

  task :restore_wd do
    Dir.chdir pwd
  end

  task :do_validation do
    puts validate_cmd.to_s
    _stdout, stderr, _status = Open3.capture3 validate_cmd
  rescue StandardError => e
    echo_exception(stderr, e)
  end

  task :docbld do
    puts 'docbld'
    _stdout, stderr, _status = Open3.capture3 pdf_cmd
  rescue StandardError => e
    echo_exception(stderr, e)
  end

  def echo_exception(_, exception)
    puts "Exception Class: #{exception.class.name}"
    puts "Exception Message: #{exception.class.message}"
    puts "Exception Backtrace: #{exception.class.backtrace}"
  end
end

# ------------------------------------------------------------------------------
