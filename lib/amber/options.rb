require 'amber/version'

module Amber 
class CommandLineOptions
  attr_accessor :verbose, :dryrun, :equipment, :filename, :writer, :language
  attr_reader :parser, :options

  WRITER_LIST = %w[Ascii LaTeX]

# ------------------------------------------------------------------------------
  def initialize
    self.verbose = false
    self.dryrun = true
    self.equipment = true 
    self.filename = []
    self.writer = "Ascii"
    self.language = nil 
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
      verbose_option parser
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
  def self.verbose_option parser
    parser.on("-v", "--verbose", "Verbose") do |v|
      @options.verbose = v
    end
  end

# ------------------------------------------------------------------------------
  def self.equipment_option parser
    parser.on("-e", "--equipment", "List Equipment") do |z|
      @options.equipment ^= z
    end
  end

# ------------------------------------------------------------------------------
  def self.plan_option parser
    parser.on("-p", "--plan x,y,x", Array, "Plan name") do |z|
      @options.filename = z.map! do |a|
        if @options.language.nil? 
          "factory/plan/#{a}/#{a}.yaml" 
        else 
          "#{@options.language}/factory/plan/#{a}/#{a}.yaml"
        end
      end 
    end
  end

# ------------------------------------------------------------------------------
  def self.suite_option parser
    parser.on("-s", "--suite x,y,x", Array, "Suite name") do |z|
      @options.filename = z.map! do |a|
        if @options.language.nil? 
          "factory/suite/#{a}/#{a}.yaml"
        else
          "#{@options.language}/factory/suite/#{a}/#{a}.yaml"
        end
      end 
    end
  end

# ------------------------------------------------------------------------------
  def self.case_option parser
    parser.on("-c", "--case x,y,x", Array, "Case name") do |z|
      @options.filename = z.map! do |a|
        if @options.language.nil? 
          "factory/case/#{a}/#{a}.yaml" 
        else
          "#{@options.language}/factory/case/#{a}/#{a}.yaml"
        end
      end 
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
      @options.writer = z
    end
  end

# ------------------------------------------------------------------------------
  def self.language_option parser
    parser.on("-l", "--language [en, fr, gr, ...]", 
              String, "Langauge name") do |z|
      @options.language = z
      puts "language=#{@options.language}"
      puts "writer=#{@options.writer}"
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
end # module
