require 'rspec'
require 'amber'
# ------------------------------------------------------------------------------

def check_conversion(input_string, output_string)
  out = Amber::StringToLatex.convert_to_latex(input_string)
  expect(out).to eq(output_string)
end

shared_examples 'convert' do |test_desc, input_string, output_string|
  context "converts #{test_desc} to LaTeX syntax:"\
    " input '#{input_string}' outputs '#{output_string}'" do
    subject { Amber::StringToLatex.convert_to_latex(input_string) }
    it { should eq(output_string) }
  end
end

describe 'String to LaTeX' do

  describe 'converts characters' do
    it_should_behave_like 'convert', 'ampersand', '&', '\\&{}'
    it_should_behave_like 'convert', 'backslash', '\\', '\\textbackslash{}'
    it_should_behave_like 'convert', 'hash', '#', '\\#{}'
    it_should_behave_like 'convert', 'dollar', '$', '\\${}'
    it_should_behave_like 'convert', 'underscore', '_', '\\_{}'
    it_should_behave_like 'convert', 'tilde', '~', '\\~{}'
    it_should_behave_like 'convert', 'caret', '^', '\\^{}'
  end

  # normal characters
  it 'does not convert the normal chars to latex syntax' do
    check_conversion(
      'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',
      'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'
    )
  end
  # punctuation marks not previously covered.
  it 'does not convert the normal punctuation to latex symbols' do
    check_conversion(
      ',.?!@*()-+=""/',
      ',.?!@*()-+=""/'
    )
  end
end
