begin
  require 'bundler/gem_tasks'
  require 'rspec/core/rake_task'

  RSpec::Core::RakeTask.new(:spec)

  task default: :spec
rescue StandardError
  puts 'RSpec is not supported on this system.'
end
