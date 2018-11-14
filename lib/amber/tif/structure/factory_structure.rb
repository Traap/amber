# frozen_string_literal: true

module Amber
  # FactoryStructure is assemble Test Plan, Test Suite, and Test Case file names
  # according the conventions Amber expects.`
  module FactoryStructure

    def self.plan_name(file)
      dirname, basename = Amber::FactoryStructure.fileparts(file)
      "factory/plan/#{dirname}/#{basename}.yaml"
    end

    def self.suite_name(file)
      dirname, basename = fileparts(file)
      "factory/suite/#{dirname}/#{basename}.yaml"
    end

    def self.case_name(file)
      dirname, basename = fileparts(file)
      "factory/case/#{dirname}/#{basename}.yaml"
    end

    private
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

