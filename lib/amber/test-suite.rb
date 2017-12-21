# {{{
module Amber 

# {{{ TestSuite 
class TestSuite < Command
  def initialize(data, options)
    super(data, options)
  end

  def install_artifact

    # TODO: Need TestSuiteData

    puts "Test Suite"
    name = @data['name']
    purpose = @data['purpose']
    requirement = @data['requirement']

    # TODO: should be including test cases.
  end

end # End TestSuite 
# -------------------------------------------------------------------------- }}}
end # module
# -------------------------------------------------------------------------- }}}
