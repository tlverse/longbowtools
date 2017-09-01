library(tltools)

args <- commandArgs(TRUE)
script <- args[1]
input_file <- args[2]

# maybe the parameter file should also have a uri to the script
cat("Script file:", script, "\n")
cat("Parameter file:", input_file, "\n")

tlparams <- ScriptParams$new(input_file)

run_and_knit(script, tlparams, run_pandoc = TRUE)
