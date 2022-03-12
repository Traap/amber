# frozen_string_literal: true

# {{{ language options

require 'amber'

# [-l | --language]
#
# [ zz | cs | da | de | en | en-US | es | fi | fr | fr-ca | fr-eu | ga | hu | it
# | nl | no | pl | pt | ro | ru | sk | sv]
# -------------------------------------------------------------------------- }}}
# {{{ 5 possible language option permutations:

# rubocop:disable Metrics.BlockLength
shared_examples 'Amber CLO language parameter' do |code, language_name|
  # rubocop:enable Metrics.BlockLength
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
end

# -------------------------------------------------------------------------- }}}
# {{{ The acutal Amber CLO Language tests.

# rubocop:disable Metrics.BlockLength
describe 'Amber CLO Language' do
  # rubocop:enable Metrics.BlockLength
  context 'no -l' do
    it 'has not been used.' do
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.language).to eq('zz')
      expect(options.language?).to be(false)
    end
  end

  # rubocop:disable Metrics.BlockLength
  describe 'with unknown language' do
    # rubocop:enable Metrics.BlockLength
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
end

# -------------------------------------------------------------------------- }}}
