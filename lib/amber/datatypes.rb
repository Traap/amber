# {{{
module Amber 

# {{{ ShellError 
class ShellError < StandardError; end
# -------------------------------------------------------------------------- }}}
# {{{ Command
class Command
  def initialize(data, options)
    @data = data
    @options = options
    @command = nil
  end

  def install_artifact; end

  def remove_artifact; end

  protected
  def do_command(included_file)
    echo_command if included_file || @options.verbose || @options.dryrun
    run_command  if included_file || !@options.dryrun
  end

  private
  def echo_command
    puts @command
  end

  def run_command
    begin
      status = system(@command)
    rescue ShellError
      abort "System command failed: #{status}"
    end
  end

end # End Command
# -------------------------------------------------------------------------- }}}

end # module
# -------------------------------------------------------------------------- }}}
