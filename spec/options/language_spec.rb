require 'rspec'
require_relative '../../lib/amber/cli/options'
require_relative '../../lib/amber/cli/command_line_options'
# ------------------------------------------------------------------------------
# language options
# [-l | --language]
#
# [zz | cs | da | de | en | es | fr-ca | fr-eu | it | ne | no | pl | ro | sv]
# ------------------------------------------------------------------------------
describe 'amber' do
  describe 'no -l' do
    it 'has not been used.' do
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.language).to eq('zz')
      expect(options.has_language?).to be(false)
    end
  end

  describe '--language=cs' do
    it 'has been used from the command line.' do
      ARGV.replace ['--language=cs']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.language).to eq('Czech')
      expect(options.has_language?).to be(true)
    end
  end

  describe '--language cs' do
    it 'has been used from the command line.' do
      ARGV.replace ['--language', 'cs']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.language).to eq('Czech')
    end
  end

  describe '-lda' do
    it 'has been used from the command line.' do
      ARGV.replace ['-lda']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.language).to eq('Dansk')
    end
  end

  describe '-l=da' do
    it 'has been used from the command line.' do
      ARGV.replace ['-l', 'da']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.language).to eq('Dansk')
    end
  end

  describe '--language=de' do
    it 'has been used from the command line.' do
      ARGV.replace ['--language=de']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.language).to eq('Deutsch')
    end
  end

  describe '--language de' do
    it 'has been used from the command line.' do
      ARGV.replace ['--language', 'de']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.language).to eq('Deutsch')
    end
  end

  describe '--language=en' do
    it 'has been used from the command line.' do
      ARGV.replace ['--language=en']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.language).to eq('English')
    end
  end

  describe '--language en' do
    it 'has been used from the command line.' do
      ARGV.replace ['--language', 'en']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.language).to eq('English')
    end
  end

  describe '--language=es' do
    it 'has been used from the command line.' do
      ARGV.replace ['--language=es']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.language).to eq('Espanol')
    end
  end

  describe '--language es' do
    it 'has been used from the command line.' do
      ARGV.replace ['--language', 'es']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.language).to eq('Espanol')
    end
  end

  describe '--language=fr-ca' do
    it 'has been used from the command line.' do
      ARGV.replace ['--language=fr-ca']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.language).to eq('CA French - Canadian')
    end
  end

  describe '--language fr-ca' do
    it 'has been used from the command line.' do
      ARGV.replace ['--language', 'fr-ca']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.language).to eq('CA French - Canadian')
    end
  end

  describe '-lfr-eu' do
    it 'has been used from the command line.' do
      ARGV.replace ['-lfr-eu']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.language).to eq('EU French - European')
    end
  end

  describe '-l fr-eu' do
    it 'has been used from the command line.' do
      ARGV.replace ['-l', 'fr-eu']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.language).to eq('EU French - European')
    end
  end

  describe '--language=it' do
    it 'has been used from the command line.' do
      ARGV.replace ['--language=it']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.language).to eq('Italiano')
    end
  end

  describe '--language it' do
    it 'has been used from the command line.' do
      ARGV.replace ['--language', 'it']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.language).to eq('Italiano')
    end
  end

  describe '-lne' do
    it 'has been used from the command line.' do
      ARGV.replace ['-lne']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.language).to eq('Nederlands')
    end
  end

  describe '-l=ne' do
    it 'has been used from the command line.' do
      ARGV.replace ['-l', 'ne']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.language).to eq('Nederlands')
    end
  end

  describe '--language=no' do
    it 'has been used from the command line.' do
      ARGV.replace ['--language=no']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.language).to eq('Norsk')
    end
  end

  describe '--language no' do
    it 'has been used from the command line.' do
      ARGV.replace ['--language', 'no']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.language).to eq('Norsk')
    end
  end

  describe '-lpl' do
    it 'has been used from the command line.' do
      ARGV.replace ['-lpl']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.language).to eq('Polish')
    end
  end

  describe '-l=pl' do
    it 'has been used from the command line.' do
      ARGV.replace ['-l', 'pl']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.language).to eq('Polish')
    end
  end

  describe '--language=ro' do
    it 'has been used from the command line.' do
      ARGV.replace ['--language=ro']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.language).to eq('Romanian')
    end
  end

  describe '--language ro' do
    it 'has been used from the command line.' do
      ARGV.replace ['--language', 'ro']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.language).to eq('Romanian')
    end
  end

  describe '-lsv' do
    it 'has been used from the command line.' do
      ARGV.replace ['-lsv']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.language).to eq('Svenska')
    end
  end

  describe '-l=sv' do
    it 'has been used from the command line.' do
      ARGV.replace ['-l', 'sv']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.language).to eq('Svenska')
    end
  end

  describe '-l sv' do
    it 'has been used from the command line.' do
      ARGV.replace ['-l', 'sv']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.language).to eq('Svenska')
    end
  end
end
