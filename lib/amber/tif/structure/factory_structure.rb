# frozen_string_literal: true

module Amber
  # FactoryStructure assembles Test Plan, Test Suite, and Test Case file names
  # in accordance with the conventions Amber expects.
  module FactoryStructure

    def self.plan_name(file)
      Amber::FactoryStructure.full_path_to_dir(file, 'plan')
    end

    def self.suite_name(file)
      Amber::FactoryStructure.full_path_to_dir(file, 'suite')
    end

    def self.case_name(file)
      Amber::FactoryStructure.full_path_to_dir(file, 'case')
    end

    private
    def self.full_path_to_dir(file, type)
      dirname, basename = Amber::FactoryStructure.fileparts(file)
      FileUtils.pwd() + "/factory/#{type}/#{dirname}/#{basename}.yaml"
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

