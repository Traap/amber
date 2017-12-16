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
      parser.banner = "Usage: emend [options]"
      parser.separator ""
      parser.separator "Specific options:"

      help_option parser
      dryrun_option parser
      app_list_option parser
      bundle_list_option parser
      file_list_option parser
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
    parser.on("-n", "--nodryrun", "No Dryrun") do |d|
      @options.dryrun ^= d
    end
  end

# ------------------------------------------------------------------------------
  def self.verbose_option parser
    parser.on("-v", "--verbose", "Verbose") do |v|
      @options.verbose = v
    end
  end

# ------------------------------------------------------------------------------
  def self.plan_list_option parser
    parser.on("-a", "--plan x,y,x", Array, "Plan name") do |apps|
      @options.filename = plan.map! {|a| "plan/#{a}/#{a}.yaml"}
    end
  end

# ------------------------------------------------------------------------------
  def self.suite_list_option parser
    parser.on("-a", "--suite x,y,x", Array, "Suite name") do |apps|
      @options.filename = suite.map! {|a| "suite/#{a}/#{a}.yaml"}
    end
  end

# ------------------------------------------------------------------------------
  def self.case_list_option parser
    parser.on("-a", "--case x,y,x", Array, "Case name") do |apps|
      @options.filename = case.map! {|a| "case/#{a}/#{a}.yaml"}
    end
  end

# ------------------------------------------------------------------------------
  def self.file_list_option parser
    parser.on("-f", "--file x,y,x", Array, "File name") do |file|
      @options.filename = file
    end
  end

# ------------------------------------------------------------------------------
  def self.version_option parser
    parser.on_tail("--version", "Show version") do
      puts Emend::VERSION
      exit
    end
  end

# ------------------------------------------------------------------------------
end # class CommandLineOptions
# ------------------------------------------------------------------------------
end # module
# ------------------------------------------------------------------------------
