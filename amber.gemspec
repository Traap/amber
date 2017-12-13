$:.unshift File.expand_path('../lib', __FILE__)
require 'amber/version'

Gem::Specification.new do |s|
  s.name          = 'amber'
  s.version       = Amber::VERSION
  s.summary       = 'amber a development environment.'
  s.description   = 'amber process YAML files to configure your computer.'
  s.authors       = ['Gary A. Howard']
  s.email         = ['gary.a.howard@mac.com']
  s.homepage      = 'https://github.com/Traap/amber'
  s.license       = 'BSD-3-Clause'

  s.require_paths = ['lib']
  s.files         = Dir['lib/**/*']
  s.test_files    = Dir['text/**/*.rb']

  s.executables   = %w(amber)

  s.required_ruby_version = '>= 1.9.3'

  s.add_development_dependency 'bundler', '~> 1.16'
  s.add_development_dependency 'rake', '~> 10.0'
  s.add_development_dependency 'test-unit', '~>3.2', '~> 3.2.3'
end
