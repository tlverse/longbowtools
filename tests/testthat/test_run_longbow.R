if(FALSE){
  setwd("~/Dropbox/gates/sprint_7D_longbow/wasting_analyses/")
  configure_cluster("~/cluster_credentials.json")
  sample_input_file <- system.file("extdata", "sample_input.json", package="longbowtools")
  rmd_filename <- system.file("examples", "example_template.Rmd", package="longbowtools")
  run_on_longbow(rmd_filename, sample_input_file, provision = FALSE)  
}