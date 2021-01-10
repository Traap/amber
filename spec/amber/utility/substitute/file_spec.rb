require 'amber'
# ------------------------------------------------------------------------------
# These Rspecs demonstrate Amber substitution capabilities relative to the
# ${file} keyword.
# ------------------------------------------------------------------------------
describe 'YAML Substitutions' do

  describe 'Amber::Substitute.file' do

    it 'can substitute ${file} with base filename from foo/bar/baz.yaml' do
      expect(Amber::Substitute
        .file('foo/bar/baz.yaml', '${file}')).to eql('baz')
    end

    it 'can substitute ${FILE} with base filename from baz/bar/foo.yaml' do
      expect(Amber::Substitute
        .file('baz/bar/foo.yaml', '${FILE}')).to eql('foo')
    end

  end
end
