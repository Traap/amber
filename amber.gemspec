$LOAD_PATH.unshift File.expand_path('lib', __dir__)
require 'amber/version'

Gem::Specification.new do |s|
  s.name          = 'amber'
  s.version       = Amber::VERSION
  s.summary       = 'Automated testing for command line programs.'
  s.description   = 'Automated testing and documentation assimulation.'
  s.authors       = ['Gary A. Howard']
  s.email         = ['gary.a.howard@mac.com']
  s.homepage      = 'https://github.com/Traap/amber'
  s.license       = 'BSD-3-Clause'

  s.require_paths = ['lib']
  s.files         = Dir['lib/**/*']
  s.test_files    = Dir['spec/**/*.rb']

  s.executables   = %w[amber]

  s.required_ruby_version = '>= 2.5'

  s.add_development_dependency 'bundler'
  s.add_development_dependency 'bundler-audit'
  s.add_development_dependency 'json'
  s.add_development_dependency 'rake'
  s.add_development_dependency 'require_all'
  s.add_development_dependency 'rspec'
  s.add_development_dependency 'rubocop'
  s.add_development_dependency 'simplecov'
end
