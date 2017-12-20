# {{{
module Amber 

# {{{ TestPlan 
class TestPlan < Command
  def initialize(data, options)
    super(data, options)
  end

  def install_artifact
    # TODO: Need TestPlanData

    puts "Test Plan"
    name = @data['name']
    purpose = @data['purpose']

    # TODO: should be including test suites.

    end
  end

  def install_on_this_os?(os)
    return true if os == "any"
    return true if RbConfig::CONFIG["host_os"].start_with? os
  end

end # End TestPlan 
# -------------------------------------------------------------------------- }}}

end # module
# -------------------------------------------------------------------------- }}}
