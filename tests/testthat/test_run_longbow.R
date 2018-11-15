if(FALSE){
  configure_cluster("~/cluster_credentials.json")
  sample_input_file <- system.file("extdata", "sample_input.json", package="longbowtools")
  rmd_filename <- system.file("examples", "example_template.Rmd", package="longbowtools")
  run_on_longbow(rmd_filename, sample_input_file, provision = TRUE)  
  sample_batch_input <- system.file("extdata", "sample_batch_input.json", package="longbowtools")
  batch_id <- run_on_longbow(rmd_filename, sample_batch_input, provision = TRUE)  
}