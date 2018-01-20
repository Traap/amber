module Amber 
  class OutputFiles 

    def initialize
      @equipment = "equipment.tex"
      @recipe = "recipe.tex"
      @recipeResults = "recipe-results-"
      @results = "results.tex"
      @stepNbr = "step-"
      @tmpPath s = s ++ "/"
    end

    # Open Recipe File.  Recipe steps are written to the recipe file in a LaTeX
    # tabular format.
    def openRecipeFile

    end

    # Open Recipe Results File.  Recipe results will be PASS or FAIL when written.
    def openRecipeResultsFile

    end

    # Recipe Results file name:  recipe-results-n.tex
    def recipeResultsFile

    end

    # Open Results File.  The results of running a recipe are written to this
    # file.
    def openResultsFile

    end

    # Open Step Results File.  Step results will be PASS or FAIL when written.
    def openStepResultsFile

    end

    # Step Results fil ename: step-n.tex
    def stepResultsFile

    end

    # Open Equipment File.  The operating system environment is written to this
    # file.
    def openEquipmentFile
      
    end

    # Open a file.
    def openFile

    end
    
  end # OutputFiles 
end # Amber
