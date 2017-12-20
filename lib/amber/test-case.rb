# {{{
module Amber 

# {{{ TestCase 
class TestCase < Command
  def initialize(data, options)
    super(data, options)
  end

  def install_artifact

    # TODO: Need TestCaseData

    puts "Test Case"
    name = @data['name']
    purpose = @data['purpose']
    requirement = @data['requirement']

    @data['steps'].each do |s|
      # TODO: Need list of commands.
      sudo = s['sudo']
      command = s['command']
      argument = s['argument']

      do_command false
    end
  end


end # End TestCase 
# -------------------------------------------------------------------------- }}}

end # module
# -------------------------------------------------------------------------- }}}
