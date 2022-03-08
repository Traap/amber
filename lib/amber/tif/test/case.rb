# frozen_string_literal: true

require 'amber/tif/test'
require 'amber/tif/test/step'
require 'amber/tof/writers/writer_factory'

module Amber
  # A test case is a collection of one or more test steps.
  class TestCase < Amber::Test
    def initialize(filename, data, options)
      super('Test Case', filename, data, options)
    end

    def run_command
      workingdir = @data['workingdir']
      workingdir += '/..' if @options.language?
      workingdir += '/..' if @options.browser?
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
