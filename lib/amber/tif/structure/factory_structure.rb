# frozen_string_literal: true

module Amber
  # FactoryStructure assembles Test Plan, Test Suite, and Test Case file names
  # in accordance with the conventions Amber expects.
  module FactoryStructure

    def self.plan_name(file)
      Amber::FactoryStructure.path_to_factory(file, 'plan')
    end

    def self.suite_name(file)
      Amber::FactoryStructure.path_to_factory(file, 'suite')
    end

    def self.case_name(file)
      Amber::FactoryStructure.path_to_factory(file, 'case')
    end

    private
    def self.path_to_factory(file, type)
      dirname, basename = Amber::FactoryStructure.fileparts(file)
      "factory/#{type}/#{dirname}/#{basename}.yaml"
    end

    def self.fileparts(file)
      # Normal pattern:
      #   factory/case/about/about.yaml
      #
      # Nested pattern:
      #   factory/case/foo/bar/baz/about.yaml
      #
      basename = File.basename(file)

      if File.dirname(file).eql? "."
        dirname = basename
      else
        dirname = file
      end
      [dirname, basename]
    end

  end
end

