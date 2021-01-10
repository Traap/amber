require 'amber'
# ------------------------------------------------------------------------------
# These Rspecs demonstrate TestEvidence operates correctly.
# ------------------------------------------------------------------------------
describe 'Test Evidence' do
  describe 'String Functions' do

    before (:all) do
      @options = Amber::Options.new
    end

    it 'does assemble a test output root without browser and language.' do
      f = Amber::TestEvidence.assemble_test_output_root(@options)
      expect(f).to eq('test-output/')
    end

    it 'does assemble a test output root with browser and language.' do
      @options.browser = 'Firefox'
      @options.language = 'Norsk'
      f = Amber::TestEvidence.assemble_test_output_root(@options)
      expect(f).to eq('test-output/Firefox/no/')
    end

    it 'does assemble tex file extension.' do
      f = Amber::TestEvidence.use_file_extension(@options)
      expect(f).to eq('.tex')
    end

    it 'does assemble ascii file extension.' do
      @options.writer = 'ascii'
      f = Amber::TestEvidence.use_file_extension(@options)
      expect(f).to eq('.txt')
    end

  end
end
