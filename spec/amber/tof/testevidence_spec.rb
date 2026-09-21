# frozen_string_literal: true

# {{{ Required files.

require 'amber'

# -------------------------------------------------------------------------- }}}
# {{{ These RSpec tests demonstrate TestEvidence operates correctly.

# rubocop:disable Metrics/BlockLength -- keeps evidence path coverage together
describe 'Test Evidence' do
  describe 'String Functions' do
    it 'does assemble a test output root without browser and language.' do
      options = Amber::CommandLineOptions.parse(ARGV)
      f = Amber::TestEvidence.assemble_test_output_root(options)
      expect(f).to eq("#{Amber::TestEvidence::TEST_OUTPUT_DIR}/")
    end

    it 'does assemble a test output root with browser and language.' do
      ARGV.replace ['--browser', 'Brave', '--language', 'no']
      options = Amber::CommandLineOptions.parse(ARGV)
      f = Amber::TestEvidence.assemble_test_output_root(options)
      expect(f).to eq("#{Amber::TestEvidence::TEST_OUTPUT_DIR}/Brave/no/")
    end

    it 'does not prefix an already absolute test output directory' do
      options = Amber::CommandLineOptions.parse(['--report-dir', 'report'])
      expected = "#{Amber::TestEvidence::TEST_OUTPUT_DIR}/"

      expect(Amber::TestEvidence.assemble_test_output_root(options)).to eq(expected)
    end

    it 'does assemble tex file extension.' do
      ARGV.replace ['--writer', 'LaTeX']
      options = Amber::CommandLineOptions.parse(ARGV)
      f = Amber::TestEvidence.use_file_extension(options)
      expect(f).to eq('.tex')
    end

    it 'does assemble ascii file extension.' do
      ARGV.replace ['--writer', 'Ascii']
      options = Amber::CommandLineOptions.parse(ARGV)
      f = Amber::TestEvidence.use_file_extension(options)
      expect(f).to eq('.txt')
    end
  end
end
# rubocop:enable Metrics/BlockLength

# ------------------------------------------------------------------------------
