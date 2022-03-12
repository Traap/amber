# frozen_string_literal: true

# {{{ Required files.

require 'amber'

# -------------------------------------------------------------------------- }}}
# {{{ Default option tests.

# rubocop:disable Metrics.BlockLength
describe 'Amber CLO Default' do
  # rubocop:enable Metrics.BlockLength

  before(:all) do
    @options = Amber::Options.new
  end

  # rubocop:disable Metrics.BlockLength
  describe 'values' do
    # rubocop:enable Metrics.BlockLength
    it "browser is #{Amber::Browser::DEFAULT}" do
      expect(@options.browser).to eq(Amber::Browser::DEFAULT)
    end

    it 'dryrun? is false.' do
      expect(@options.dryrun?).to be(true)
    end

    it 'dump? is false.' do
      expect(@options.dump?).to be(false)
    end

    it 'filename is nil.' do
      expect(@options.filename).to eq(nil)
    end

    it 'files is nil.' do
      expect(@options.files).to eq(nil)
    end

    it 'files? is false' do
      expect(@options.files?).to eq(false)
    end

    it "language is #{Amber::Language::DEFAULT}" do
      expect(@options.language).to eq(Amber::Language::DEFAULT)
    end

    it 'browser? is false.' do
      expect(@options.browser?).to eq(false)
    end

    it 'language? is false.' do
      expect(@options.language?).to eq(false)
    end

    it 'log_command? is false.' do
      expect(@options.log_command?).to eq(false)
    end

    it 'log_environment? is false.' do
      expect(@options.log_environment?).to eq(false)
    end

    it 'log_requirement? is false.' do
      expect(@options.log_requirement?).to eq(false)
    end

    it 'obliterate? is false.' do
      expect(@options.obliterate?).to be(false)
    end

    it 'parser is nil.' do
      expect(@options.parser).to eq(nil)
    end

    it 'run? is false.' do
      expect(@options.run?).to be(false)
    end

    it 'simulate? is false.' do
      expect(@options.simulate?).to be(false)
    end

    it 'test_case is nil.' do
      expect(@options.test_case).to be(nil)
    end

    it 'test_plan is nil.' do
      expect(@options.test_plan).to be(nil)
    end

    it 'test_suite is nil.' do
      expect(@options.test_suite).to be(nil)
    end

    it 'verbose? is false.' do
      expect(@options.verbose?).to be(false)
    end

    it "version is #{Amber::VERSION}" do
      expect(@options.version).to eq(Amber::VERSION)
    end

    it "writer is #{Amber::Writer::DEFAULT}" do
      expect(@options.writer).to eq(Amber::Writer::DEFAULT)
    end
  end
end

# -------------------------------------------------------------------------- }}}
