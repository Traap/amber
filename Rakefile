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
  pdfCmd = 'latexmk -pdf -silent example.texx'
  pwd = ''

  task :amber => [:save_dir, :report_dir, :run_validation, :make_pdf]

  task :save_dir do
    pwd = Dir.getwd
  end

  task :report_dir do
    Dir.chdir reportDir 
  end

  task :return_to_dir do
    Dir.chdir pwd
  end

  task :run_validation do
    puts "#{validateCmd}"
    begin
      _stdout, stderr, _status = Open3.capture3 validateCmd 
    rescue StandardError => e 
      echo_exception(stderr, e)
    end
  end

  task :make_pdf do
    puts "#{pdfCmd}"
    begin
      _stdout, stderr, _status = Open3.capture3 pdfCmd 
    rescue StandardError => e 
      echo_exception(stderr, e)
    end
  end

  def echo_exception(s, e) 
    puts "Exception Class: #{e.class.name}"
    puts "Exception Message: #{e.class.message}"
    puts "Exception Backtrace: #{e.class.backtrace}"
  end

end

# ------------------------------------------------------------------------------
