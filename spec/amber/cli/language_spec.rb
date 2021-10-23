# frozen_string_literal: true

require 'amber'
# ------------------------------------------------------------------------------
# language options
# [-l | --language]
#
# [ zz | cs | da | de | en | en-US | es | fi | fr | fr-ca | fr-eu | ga | hu | it
# | nl | no | pl | pt | ro | ru | sk | sv]
# ------------------------------------------------------------------------------

shared_examples 'Amber CLO language parameter' do |code, language_name|
  # {{{ 5 possible language option permutations:

  context "--language=#{code}" do
    it "returns #{language_name} when run with double dash and equal sign" do
      ARGV.replace ["--language=#{code}"]
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.language).to eq(language_name)
      expect(options.language?).to be(true)
    end
  end

  context "--language #{code}" do
    it "returns #{language_name} when run with double dash and a space" do
      ARGV.replace ['--language', code]
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.language).to eq(language_name)
      expect(options.language?).to be(true)
    end
  end

  context "-l#{code}" do
    it "returns #{language_name} when run with dash and no space" do
      ARGV.replace ["-l#{code}"]
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.language).to eq(language_name)
      expect(options.language?).to be(true)
    end
  end

  # The "-l=#{code}" examples fail in rspec with ['-l=#{code}'] as invalid
  # parameter. They have been removed.

  context "-l #{code}" do
    it "returns #{language_name} when run with dash and a space" do
      ARGV.replace ['-l', code]
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.language).to eq(language_name)
      expect(options.language?).to be(true)
    end
  end

  # ------------------------------------------------------------------------ }}}
end

describe 'Amber CLO Language' do
  # {{{ The acutal Amber CLO Language tests.

  describe 'no -l' do
    it 'has not been used.' do
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.language).to eq('zz')
      expect(options.language?).to be(false)
    end
  end

  describe 'with unknown language' do
    context '--language XX' do
      it 'raises an invalid argument exception' do
        ARGV.replace ['--language', 'XX']
        expect { Amber::CommandLineOptions.parse(ARGV).to }
        raise_exception(RuntimeError, /invalid argument/)
      end
    end

    context '--language=XX' do
      it 'raises an invalid argument exception' do
        ARGV.replace ['--language=XX']
        expect { Amber::CommandLineOptions.parse(ARGV).to }
        raise_exception(RuntimeError, /invalid argument/)
      end
    end

    context '-l XX' do
      it 'raises an invalid argument exception' do
        ARGV.replace ['-l', 'XX']
        expect { Amber::CommandLineOptions.parse(ARGV).to }
        raise_exception(RuntimeError, /invalid argument/)
      end
    end

    context '-lXX' do
      it 'raises an invalid argument exception' do
        ARGV.replace ['-lXX']
        expect { Amber::CommandLineOptions.parse(ARGV).to }
        raise_exception(RuntimeError, /invalid argument/)
      end
    end
  end

  Amber::Language::CODE.each do |code, language_name|
    it_behaves_like 'Amber CLO language parameter', code, language_name
  end

  # ------------------------------------------------------------------------ }}}
end
