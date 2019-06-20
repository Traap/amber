# frozen_string_literal: true

require 'amber/tif/test'
require 'amber/tif/test/step'
require 'amber/tof/writers/writer_factory'

module Amber
  # Decorate test case output with LaTeX text.
  class TestCase < Amber::Test
    def initialize(filename, data, options)
      super('Test Case', filename, data, options)
    end

    def run_command
      workingdir = @data['workingdir']
      workingdir += "/.." if @options.has_language?
      workingdir += "/.." if @options.has_browser?
      nbr = 0
      @data['steps'].each do |s|
        nbr += 1
        step = Amber::WriterFactory.get_test_step(
          @filename, @data, @options, s, nbr, workingdir
        )
        step.process
      end
    end
  end
end
