module Amber 
# ------------------------------------------------------------------------------
class CommandLineOptions
  attr_accessor :verbose, :dryrun, :filename
  attr_reader :parser, :options

# ------------------------------------------------------------------------------
  def initialize
    self.verbose = false
    self.dryrun = true
    self.filename = []
  end

# ------------------------------------------------------------------------------
  def self.parse args
    @options = CommandLineOptions.new
    option_parser.parse! args
    @options
  end

# ------------------------------------------------------------------------------
  def self.option_parser
    @parser ||= OptionParser.new do |parser|
      parser.banner = "Usage: amber [options]"
      parser.separator ""
      parser.separator "Specific options:"

      help_option parser
      dryrun_option parser
      plan_option parser
      suite_option parser
      case_option parser
      file_option parser
      verbose_option parser
      version_option parser
    end
  end

# ------------------------------------------------------------------------------
  def self.help_option parser
    parser.on_tail("-h", "--help", "Show this message") do
      puts parser
      exit
    end
  end

# ------------------------------------------------------------------------------
  def self.dryrun_option parser
    parser.on("-n", "--nodryrun", "No Dryrun") do |z|
      @options.dryrun ^= z
    end
  end

# ------------------------------------------------------------------------------
  def self.verbose_option parser
    parser.on("-v", "--verbose", "Verbose") do |z|
      @options.verbose = z
    end
  end

# ------------------------------------------------------------------------------
  def self.plan_option parser
    parser.on("-p", "--plan x,y,x", Array, "Plan name") do |z|
      @options.filename = z.map! {|a| "plan/#{a}/#{a}.yaml"}
    end
  end

# ------------------------------------------------------------------------------
  def self.suite_option parser
    parser.on("-s", "--suite x,y,x", Array, "Suite name") do |z|
      @options.filename = z.map! {|a| "suite/#{a}/#{a}.yaml"}
    end
  end

# ------------------------------------------------------------------------------
  def self.case_option parser
    parser.on("-c", "--case x,y,x", Array, "Case name") do |z|
      @options.filename = z.map! {|a| "case/#{a}/#{a}.yaml"}
    end
  end

# ------------------------------------------------------------------------------
  def self.file_option parser
    parser.on("-f", "--file x,y,x", Array, "File name") do |z|
      @options.filename = file
    end
  end

# ------------------------------------------------------------------------------
  def self.version_option parser
    parser.on_tail("--version", "Show version") do
      puts Amber::VERSION
      exit
    end
  end

# ------------------------------------------------------------------------------
end # class CommandLineOptions
# ------------------------------------------------------------------------------
end # module
# ------------------------------------------------------------------------------
