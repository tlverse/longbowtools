library(tltools)
library(testthat)
context("basic test")

load_all()
sample_input_file <- fpath <- system.file("extdata", "sample_input.json", package="tltools")

tlparams <- ScriptParams$new(sample_input_file)
tlparams$params

expect_equal(dim(tlparams$data),c(1912,37))
expect_true(dir.exists(tlparams$output_dir))
