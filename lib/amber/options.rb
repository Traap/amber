require 'amber/version' 
require 'amber/browser' 
require 'amber/language' 
require 'amber/writer' 

module Amber 

  class CommandLineOptions
    attr_accessor :browser, :dryrun, :equipment, :filename, :language, :options,
                  :parser, :verbose, :writer 

# ------------------------------------------------------------------------------
  def initialize
    self.browser = Amber::Browser::Default 
    self.dryrun = true
    self.equipment = false 
    self.filename = []
    self.language = Amber::Language::Default
    self.options = nil
    self.parser = nil
    self.verbose = false
    self.writer = Amber::Writer::Default 
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

      browser_option parser
      dryrun_option parser
      equipment_option parser
      file_option parser
      help_option parser
      language_option parser
      verbose_option parser
      version_option parser
      writer_option parser

      plan_option parser
      suite_option parser
      case_option parser

      dump_option parser
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
  def self.equipment_option parser
    parser.on("-e", "--equipment", "List Equipment") do |z|
      @options.equipment = z
    end
  end

# ------------------------------------------------------------------------------
  def self.plan_option parser
    parser.on("-p", "--plan x,y,x", Array, "Plan name") do |z|
      @options.filename = z.map! do |a|
        "factory/plan/#{a}/#{a}.yaml" 
      end 
    end
  end

# ------------------------------------------------------------------------------
  def self.suite_option parser
    parser.on("-s", "--suite x,y,x", Array, "Suite name") do |z|
      @options.filename = z.map! do |a|
        "factory/suite/#{a}/#{a}.yaml"
      end 
    end
  end

# ------------------------------------------------------------------------------
  def self.case_option parser
    parser.on("-c", "--case x,y,x", Array, "Case name") do |z|
      @options.filename = z.map! do |a|
        "factory/case/#{a}/#{a}.yaml" 
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
    parser.on("-w", "--writer WRITER", String, Amber::Writer::Names,
              "Select writer", "#{Amber::Writer::Names}") do |z|
      @options.writer = z
    end
  end

# ------------------------------------------------------------------------------
  def self.browser_option parser
    parser.on("-b", "--browser BROWSER", String, Amber::Browser::Names,
              "Select Browser", "#{Amber::Browser::Names}") do |z|
      @options.browser = z
    end
  end


# ------------------------------------------------------------------------------
  def self.language_option parser
    parser.on("--language LANGUAGE", 
              Amber::Language::Names, Amber::Language::Code,
              "Select language", "#{Amber::Language::Names}") do |z|
      @options.language = z
    end
  end

# ------------------------------------------------------------------------------
  def self.dump_option parser
    parser.on_tail("-d", "--dump", "Dump options (must be last).") do
      puts "  browser: #{@options.browser}"
      puts "   dryrun: #{@options.dryrun}"
      puts "equipment: #{@options.equipment}"
      puts " filename: #{@options.filename}"
      puts " language: #{@options.language}"
      puts "  verbose: #{@options.verbose}"
      puts "   writer: #{@options.writer}"
      exit
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
