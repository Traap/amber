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

    echo_test_case_header name, purpose, requirement

    @data['steps'].each do |s|
      # TODO: Need list of commands.
      sudo = s['sudo']
      command = s['command']
      argument = s['argument']

      echo_test_case_command sudo, command, argument

#      do_command false
    end
  end

  def echo_test_case_header(name, purpose, requirement)
    puts "           Name: #{name}"
    puts "        Purpose: #{purpose}"
    puts "    Requirement: #{requirement}"
  end

  def echo_test_case_command(sudo, command, argument)
    puts "        Command: #{sudo} #{command} #{argument}"
  end


end # End TestCase 
# -------------------------------------------------------------------------- }}}

end # module
# -------------------------------------------------------------------------- }}}
