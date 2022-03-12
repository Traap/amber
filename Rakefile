# frozen_string_literal: true

# {{{ Intended Use
#
# You have three basic options as follows:
#   1) rake
#      without options will run rspec.
#
#   2) rake build:amber
#      builds, rspecs, and installs Amber.
#
#   3) rake validate:amber
#      runs Amber's validtion plan and assembles a report using docbld.
#
# -------------------------------------------------------------------------- }}}
# {{{ Required files.

require 'open3'
require 'bundler/gem_tasks'
require 'rspec/core/rake_task'

# -------------------------------------------------------------------------- }}}
# {{{ Prerequisite checks.

if ENV['AMBERPATH'].empty?
  puts 'WARNING: Amber is not installed.'
  abort = true
end

if ENV['DOCBLDPATH'].empty?
  puts 'WARNING: docbld is not installed.'
  abort = true
end

exit if abort

# -------------------------------------------------------------------------- }}}
# {{{ Customize Amber build and validate variables.

report_dir = "#{ENV['AMBERPATH']}/report"
validate_cmd = 'amber --nodryrun --log-environment --obliterate --plan=master'
pdf_cmd = "rake --rakefile #{ENV['DOCBLDPATH']}/Rakefile"
pwd = ''

# -------------------------------------------------------------------------- }}}
# {{{ Build Amber.

namespace :build do
  task :amber do
    system 'bundle install'
    system 'bundle exec rake'
    system 'bundle exec rake install'
  end
end

# -------------------------------------------------------------------------- }}}
# {{{ Validate Amber.

# rubocop:disable Metrics.BlockLength
namespace :validate do
  # rubocop:enable Metrics.BlockLength

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

# -------------------------------------------------------------------------- }}}
# {{{ Run rspecs.

begin
  RSpec::Core::RakeTask.new(:spec)
  task default: :spec
rescue StandardError
  puts 'RSpec is not supported on this system.'
  exit
end

# -------------------------------------------------------------------------- }}}
