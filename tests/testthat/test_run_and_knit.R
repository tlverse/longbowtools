library(tltools)
library(testthat)
context("run and knit")

sample_input_file <- fpath <- system.file("extdata", "sample_input.json", package="tltools")

tlparams <- ScriptParams$new(sample_input_file)

Rmd_file <- fpath <- system.file("examples", "sl3.Rmd", package="tltools")

run_and_knit(Rmd_file, tlparams, run_pandoc = TRUE)
