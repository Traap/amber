require 'rspec'
require 'amber'
# ------------------------------------------------------------------------------
# These RSpecs demonstrate Amber substitution capabilities.
# ------------------------------------------------------------------------------
describe 'YAML file Substitution' do

  before(:all) do
    @options = Amber::Options.new
  end

  # ----------------------------------------------------------------------------
  describe 'File name' do

    it 'can substitute ${file} with base filename from foo/bar/baz.yaml' do
      expect(Amber::Substitute
        .file('foo/bar/baz.txt', '${file}')).to eql('baz')
    end
  end

  # ----------------------------------------------------------------------------
  describe 'Home path' do

    it 'can substitute ${home} to ~' do
      expect(Amber::Substitute.home("${home}")).to eql('~')
    end

    it 'can substitute ${home} to ~' do
      expect(Amber::Substitute.home("${home}")).to eql('~')
    end

    it 'can substitute ${home} and ${HOME} to ~ and ~' do
      expect(Amber::Substitute.home("${home} and ${HOME}")).to eql('~ and ~')
    end

  end
 
  # ----------------------------------------------------------------------------
  describe 'Expand path' do
    
    it "can expand ~ to #{Dir.home}" do
      expect(Amber::Substitute.expand_path('~').strip).to eql(Dir.home)
    end

    it "can expand ~ and ~ to #{Dir.home} and #{Dir.home}" do
      expect(Amber::Substitute
        .expand_path('~ and ~').strip).to eql("#{Dir.home} and #{Dir.home}")
    end

  end

  # ----------------------------------------------------------------------------
  describe 'Browser' do

    it "can substitute ${BROWSER} to #{Amber::Browser::DEFAULT}" do
      expect(Amber::Substitute
        .browser(@options, "${BROWSER}")).to eql(Amber::Browser::DEFAULT)
    end

    it "can substitute ${browser} to #{Amber::Browser::DEFAULT}" do
      expect(Amber::Substitute
        .browser(@options, "${browser}")).to eql(Amber::Browser::DEFAULT)
    end

    it 'can substitute ${BROWSER} to Chrome' do
      @options.browser = 'Chrome' 
      expect(Amber::Substitute
        .browser(@options, '${BROWSER}')).to eql('Chrome')
    end

    it 'can substitute ${browser} to Edge' do
      @options.browser = 'Edge' 
      expect(Amber::Substitute
        .browser(@options, '${browser}')).to eql('Edge')
    end

    it 'can substitute ${BROWSER} to Firefox' do
      @options.browser = 'Firefox' 
      expect(Amber::Substitute
        .browser(@options, '${BROWSER}')).to eql('Firefox')
    end

    it 'can substitute ${browser} to IE' do
      @options.browser = 'IE'
      expect(Amber::Substitute
        .browser(@options, '${browser}')).to eql('IE')
    end

    it 'can substitute ${BROWSER} to Opera' do
      @options.browser = 'Opera' 
      expect(Amber::Substitute
        .browser(@options, '${BROWSER}')).to eql('Opera')
    end

  end

end

