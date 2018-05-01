#' Run template with parameters as specified and generate markdown output
#' @param rmd_filename the script to template
#' @param params_filename a json file specifying the parameters to use
#' @param output_directory a folder to work in
#' @export
#' @importFrom rmarkdown render pandoc_convert html_document
#' @importFrom jsonlite fromJSON
#' @rdname run_analysis
run_internal <- function(rmd_filename, params_filename, output_directory = tempdir()){
  args = commandArgs(trailingOnly=TRUE)
  
  #copy Rmd and params file to output directory
  working_rmd_filename <- file.path(output_directory, "REPORT.Rmd")
  file.copy(rmd_filename, working_rmd_filename)
  
  inputs_filename <- file.path(output_directory, "inputs.json")
  file.copy(params_filename, inputs_filename)
  
  html_filename <- file.path(output_directory, "REPORT.html")
  
  params_ <- fromJSON(params_filename)
  params_$output_directory <- output_directory
  rmarkdown::render(working_rmd_filename, params=params_, 
                    output_file=html_filename, output_dir=output_directory,
                    output_format = html_document(self_contained = TRUE, keep_md = TRUE),
                    knit_root_dir = output_directory)

  
  return(html_filename)
}  

#' @rdname run_analysis
#' @param open_result if \code{TRUE}, opens compiled report in browser
#' @importFrom utils browseURL
#' @export
run_locally <- function(rmd_filename, params_filename, output_directory=tempdir(), open_result = TRUE){
  pandoc_filename <- run_internal(rmd_filename, params_filename, output_directory=output_directory)
  if(open_result){
    browseURL(pandoc_filename)
  }
}  