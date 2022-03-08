# frozen_string_literal: true

module Amber
  # FactoryStructure assembles Test Plan, Test Suite, and Test Case file names
  # following the conventions Amber expects.
  module FactoryStructure
    # {{{ plan_name

    def self.plan_name(file)
      Amber::FactoryStructure.path_to_factory(file, 'plan')
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ suite_name

    def self.suite_name(file)
      Amber::FactoryStructure.path_to_factory(file, 'suite')
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ case_name

    def self.case_name(file)
      Amber::FactoryStructure.path_to_factory(file, 'case')
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ path_to_factory

    def self.path_to_factory(file, type)
      dirname, basename = Amber::FactoryStructure.fileparts(file)
      "factory/#{type}/#{dirname}/#{basename}.yaml"
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ fileparts

    def self.fileparts(file)
      # Normal pattern:
      #   factory/case/about/about.yaml
      #
      # Nested pattern:
      #   factory/case/foo/bar/baz/about.yaml
      #
      basename = File.basename(file)

      dirname = if File.dirname(file).eql? '.'
                  basename
                else
                  file
                end
      [dirname, basename]
    end

    # ---------------------------------------------------------------------- }}}
  end
end
