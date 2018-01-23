library(tltools)
library(testthat)
context("run and knit")

sample_input_file <- system.file("extdata", "sample_input.json", package="tltools")
rmd_filename <- system.file("examples", "example_template.Rmd", package="tltools")
temp_sample_json <- tempfile()
params <- params_from_rmd(rmd_filename, temp_sample_json)
result <- run_internal(rmd_filename, temp_sample_json)
result2 <- run_locally(rmd_filename, temp_sample_json, open_result = FALSE)
# result2 <- run_locally(rmd_filename, temp_sample_json, open_result = TRUE)

# todo: not sure how to test this without putting ghap credentials here 
# configure_ghap("username", "password", "ip", "token")
# run_on_cluster(rmd_filename, temp_sample_json, open_result = TRUE)

