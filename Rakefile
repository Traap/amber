require 'open3'
require 'bundler/gem_tasks'
require 'rspec/core/rake_task'

# ------------------------------------------------------------------------------

begin
  RSpec::Core::RakeTask.new(:spec)
  task default: :spec
rescue StandardError
  puts 'RSpec is not supported on this system.'
end

# ------------------------------------------------------------------------------

namespace :gem do
  task :build do
    system "bundle install"
    system "bundle exec rake"
    system "bundle exec rake install"
  end
end

# ------------------------------------------------------------------------------

namespace :validate do

  reportDir = ENV['AMBERPATH'] + '/report'
  validateCmd = 'amber --nodryrun --environment --obliterate --plan=master'
  pdfCmd = 'rake --rakefile ' + ENV['DOCBLDPATH'] + '/Rakefile'
  pwd = ''

  task :amber => [:save_wd, :report_dir, :do_validation, :restore_wd, :docbld]

  task :save_wd do
    pwd = Dir.getwd
  end

  task :report_dir do
    Dir.chdir reportDir 
  end

  task :restore_wd do
    Dir.chdir pwd
  end

  task :do_validation do
    begin
      puts "#{validateCmd}"
      _stdout, stderr, _status = Open3.capture3 validateCmd 
    rescue StandardError => e 
      echo_exception(stderr, e)
    end
  end

  task :docbld do
    if ENV['DOCBLDPATH'].length > 0
      begin
        puts "docbld"
        _stdout, stderr, _status = Open3.capture3 pdfCmd 
      rescue StandardError => e 
        echo_exception(stderr, e)
      end
    else
      puts "WARNING: docbld is not installed."
    end
  end

  def echo_exception(s, e) 
    puts "Exception Class: #{e.class.name}"
    puts "Exception Message: #{e.class.message}"
    puts "Exception Backtrace: #{e.class.backtrace}"
  end

end

# ------------------------------------------------------------------------------
