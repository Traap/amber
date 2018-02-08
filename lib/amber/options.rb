module Amber 
# ------------------------------------------------------------------------------
class CommandLineOptions
  attr_accessor :dryrun, :equipment, :filename, :writer, :language
  attr_reader :parser, :options

  WRITER_LIST = %w[Ascii LaTeX]

# ------------------------------------------------------------------------------
  def initialize
    self.dryrun = true
    self.equipment = true 
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
      equipment_option parser
      file_option parser
      language_option parser
      writer_option parser
      version_option parser

      plan_option parser
      suite_option parser
      case_option parser
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
  def self.equipment_option parser
    parser.on("-n", "--equipment", "List Equipment") do |z|
      @options.equipment ^= z
    end
  end

# ------------------------------------------------------------------------------
  def self.plan_option parser
    parser.on("-p", "--plan x,y,x", Array, "Plan name") do |z|
      @options.filename = z.map! {|a| "factory/plan/#{a}/#{a}.yaml"}
    end
  end

# ------------------------------------------------------------------------------
  def self.suite_option parser
    parser.on("-s", "--suite x,y,x", Array, "Suite name") do |z|
      @options.filename = z.map! {|a| "factory/suite/#{a}/#{a}.yaml"}
    end
  end

# ------------------------------------------------------------------------------
  def self.case_option parser
    parser.on("-c", "--case x,y,x", Array, "Case name") do |z|
      @options.filename = z.map! {|a| "factory/case/#{a}/#{a}.yaml"}
    end
  end

# ------------------------------------------------------------------------------
  def self.file_option parser
    parser.on("-f", "--file x,y,x", Array, "File name") do |z|
      @options.filename = z.map! {|a| "#{a}"}
    end
  end

# ------------------------------------------------------------------------------
  def self.writer_option parser
    parser.on("-w", "--writer WRITER", String, WRITER_LIST,
              "Select writer", "#{WRITER_LIST}") do |z|
      @options.writer = z  end
  end

# ------------------------------------------------------------------------------
  def self.language_option parser
    parser.on("-r", "--Language [EN, FR, GR, ...]", 
              String, "Langauge name") do |z|
      @options.language = z 
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
  def self.language_option parser
  end

# ------------------------------------------------------------------------------
end # class CommandLineOptions
# ------------------------------------------------------------------------------
end # module
# ------------------------------------------------------------------------------
