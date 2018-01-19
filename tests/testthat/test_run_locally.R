library(tltools)
library(testthat)
context("run and knit")

sample_input_file <- fpath <- system.file("extdata", "sample_input.json", package="tltools")
Rmd_file <- fpath <- system.file("examples", "sl3.Rmd", package="tltools")


result <- run_locally(Rmd_file, sample_input_file)
