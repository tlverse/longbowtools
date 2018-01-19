#' Run script with parameters as specified and generate markdown output
#' @param rmd_filename the script to run
#' @param params_filename a json file specifying the parameters to use
#' @param output_directory a folder to work in
#' @export
#' @importFrom rmarkdown render
#' @importFrom knitr pandoc
run_internal <- function(rmd_filename, params_filename, output_directory = tempdir()){
  args = commandArgs(trailingOnly=TRUE)
  
  output_filename <- file.path(output_directory, "REPORT.md")
  
  params_ <- fromJSON(params_filename)
  
  owd <- setwd(output_directory)
  
  try({
    rmarkdown::render(rmd_filename, output_file=output_filename, params=params_)
    pandoc_filename <- pandoc(output_filename)
  })
  setwd(owd)
  
  return(pandoc_filename)
}  

run_locally <- function(rmd_filename, params_filename, open = TRUE){
  pandoc_filename <- run_internal(rmd_filename, params_filename)
  system(sprintf("open %s", pandoc_filename))
  
}  
