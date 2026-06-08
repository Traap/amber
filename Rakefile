# frozen_string_literal: true

# {{{ Intended Use
#
# You have three basic options as follows:
#   1) rake
#      without options will run RSpec.
#
#   2) rake build:amber
#      builds, runs RSpec, and installs Amber.
#
#   3) rake validate:amber
#      runs Amber's validation plan and assembles a report using docbld.
#
# -------------------------------------------------------------------------- }}}
# {{{ Required files.

require 'open3'
require 'bundler/gem_tasks'
require 'rspec/core/rake_task'
require 'shellwords'

# -------------------------------------------------------------------------- }}}
# {{{ Customize Amber build and validate variables.

validate_cmd = lambda {
  [File.join(ENV.fetch('AMBERPATH'), 'bin', 'amber'),
   '--nodryrun', '--log-environment', '--obliterate', '--plan=master']
}
pdf_cmd = -> { ['rake', '--rakefile', File.join(ENV.fetch('DOCBLDPATH'), 'Rakefile'), '--build-all', 'deploy'] }
pwd = ''

# -------------------------------------------------------------------------- }}}
# {{{ Build Amber.

namespace :build do
  task :amber do
    sh 'bundle install'
    sh 'bundle exec rake'
    sh 'bundle exec rake install'
    sh 'gem cleanup amber'
  end
end

# -------------------------------------------------------------------------- }}}
# {{{ Validate Amber.

# rubocop:disable Metrics.BlockLength
namespace :validate do
  # rubocop:enable Metrics.BlockLength

  task amber: %i[check_env save_wd report_dir do_validation restore_wd docbld]

  task run:   %i[check_env save_wd report_dir do_validation restore_wd]

  task :check_env do
    require_env('AMBERPATH')
    require_env('DOCBLDPATH')
    abort "#{amber_bin} must be executable." unless File.executable?(amber_bin)
  end

  task :save_wd do
    pwd = Dir.getwd
  end

  task :report_dir do
    Dir.chdir File.join(ENV.fetch('AMBERPATH'), 'report')
  end

  task :restore_wd do
    Dir.chdir pwd
  end

  task :do_validation do
    run_command!(validate_cmd.call)
  end

  task :docbld do
    run_command!(pdf_cmd.call)
  end

  def require_env(name)
    abort "#{name} must be defined." if ENV[name].to_s.empty?
  end

  def amber_bin
    File.join(ENV.fetch('AMBERPATH'), 'bin', 'amber')
  end

  def run_command!(command)
    puts command.shelljoin
    stdout, stderr, status = Open3.capture3(*command)
    puts stdout unless stdout.empty?
    warn stderr unless stderr.empty?
    abort "Command failed with status #{status.exitstatus}: #{command.shelljoin}" unless status.success?
  end
end

# -------------------------------------------------------------------------- }}}
# {{{ Run RSpec.

begin
  RSpec::Core::RakeTask.new(:spec)
  task default: :spec
rescue StandardError
  puts 'RSpec is not supported on this system.'
  exit
end

# -------------------------------------------------------------------------- }}}
