require "amber"
# ------------------------------------------------------------------------------
# These Rspecs demonstrate TestEvidence operates correctly.
# ------------------------------------------------------------------------------
describe "Test Evidence" do
  describe "String Functions" do

    it "does assemble a test output root without browser and language." do
      options = Amber::CommandLineOptions.parse(ARGV)
      f = Amber::TestEvidence.assemble_test_output_root(options)
      expect(f).to eq("test-output/")
    end

    it "does assemble a test output root with browser and language." do
      ARGV.replace ["--browser", "Brave", "--language", "no"]
      options = Amber::CommandLineOptions.parse(ARGV)
      f = Amber::TestEvidence.assemble_test_output_root(options)
      expect(f).to eq("test-output/Brave/no/")
    end

    it "does assemble tex file extension." do
      ARGV.replace ["--writer",   "LaTeX"]
      options = Amber::CommandLineOptions.parse(ARGV)
      f = Amber::TestEvidence.use_file_extension(options)
      expect(f).to eq(".tex")
    end

    it "does assemble ascii file extension." do
      ARGV.replace ["--writer",   "Ascii"]
      options = Amber::CommandLineOptions.parse(ARGV)
      f = Amber::TestEvidence.use_file_extension(options)
      expect(f).to eq(".txt")
    end

  end
end
