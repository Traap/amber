# frozen_string_literal: true

# {{{ Required files.

require 'amber'

# -------------------------------------------------------------------------- }}}
# {{{ Environment options.
#
# [-e | --environment]
#
# -------------------------------------------------------------------------- }}}
# {{{ Environment tests

describe 'Amber CLO Environment' do
  describe 'no -e' do
    it 'has not been used.' do
      options = Amber::Options.new
      expect(options.log_environment?).to be(false)
    end
  end

  describe '-e' do
    it 'has been used from the command line.' do
      ARGV.replace ['-e']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.log_environment?).to be(true)
    end
  end

  describe '--environment' do
    it 'has been used from the command line.' do
      ARGV.replace ['--log-environment']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.log_environment?).to be(true)
    end
  end
end

describe 'Amber CLO Environment' do
  it 'dollar_signs are escaped.' do
    o_texts = ['\$Test$', '\#Test', '_Test', '\\Test']
    e_texts = ['/\\$Test\\$', '/\\#Test', '-Test', '/Test']
    n_texts = []
    environment = Amber::LaTeXEnvironment.new(
      Amber::Environment.new, Amber::Options.new
    )
    o_texts.each do |text|
      n_texts.append(environment.send(:replace_characters, text))
    end

    # When needed, uncommend to aid debugging.
    # puts("o_texts: #{o_texts}\r")
    # puts("e_texts: #{e_texts}\r")
    # puts("n_texts: #{n_texts}\r")

    expect(n_texts == e_texts).to be(true)
  end
end

# -------------------------------------------------------------------------- }}}
