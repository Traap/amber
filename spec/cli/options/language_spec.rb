require 'rspec'
require 'amber'
# ------------------------------------------------------------------------------
# language options
# [-l | --language]
#
# [zz | cs | da | de | en | en-US | es | fr-ca | fr-eu | hu | it | ne | no | pl | pt| ro | sk | sv]
# ------------------------------------------------------------------------------

shared_examples 'CLO language parameter' do |code, language_name|
  # 5 possible language option permutations:
  context "--language=#{code}" do
    it "returns #{language_name} when run with double dash and equal sign" do
      ARGV.replace ["--language=#{code}"]
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.language).to eq(language_name)
      expect(options.has_language?).to be(true)
    end
  end

  context "--language #{code}" do
    it "returns #{language_name} when run with double dash and a space" do
      ARGV.replace ['--language', code]
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.language).to eq(language_name)
      expect(options.has_language?).to be(true)
    end
  end

  context "-l#{code}" do
    it "returns #{language_name} when run with dash and no space" do
      ARGV.replace ["-l#{code}"]
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.language).to eq(language_name)
      expect(options.has_language?).to be(true)
    end
  end

  # The "-l=#{code}" examples fail in rspec with ['-l=#{code}'] as invalid parameter. They have been removed.

  context "-l #{code}" do
    it "returns #{language_name} when run with dash and a space" do
      ARGV.replace ['-l', code]
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.language).to eq(language_name)
      expect(options.has_language?).to be(true)
    end
  end
end

describe 'Amber CLO Language' do

  describe 'no -l' do
    it 'has not been used.' do
      options = Amber::Options.new
      expect(options.language).to eq('zz')
      expect(options.has_language?).to be(false)
    end
  end

  describe 'with unknown language' do
    context "--language XX" do
      it "raises an invalid argument exception" do
        ARGV.replace ['--language', 'XX']
        expect { Amber::CommandLineOptions.parse(ARGV) }.to raise_exception(RuntimeError, /invalid argument/)
      end
    end
    context "--language=XX" do
      it "raises an invalid argument exception" do
        ARGV.replace ['--language=XX']
        expect { Amber::CommandLineOptions.parse(ARGV) }.to raise_exception(RuntimeError, /invalid argument/)
      end
    end
    context "-l XX" do
      it "raises an invalid argument exception" do
        ARGV.replace ['-l', 'XX']
        expect { Amber::CommandLineOptions.parse(ARGV) }.to raise_exception(RuntimeError, /invalid argument/)
      end
    end
    context "-lXX" do
      it "raises an invalid argument exception" do
        ARGV.replace ['-lXX']
        expect { Amber::CommandLineOptions.parse(ARGV) }.to raise_exception(RuntimeError, /invalid argument/)
      end
    end
  end

  Amber::Language::CODE.each do |code, language_name|
    it_behaves_like 'CLO language parameter', code, language_name
  end
end
