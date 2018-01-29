#' Run script with parameters as specified and generate markdown output
#' @param rmd_filename the script to run
#' @param params_filename a json file specifying the parameters to use
#' @param output_directory a folder to work in
#' @export
#' @importFrom rmarkdown render pandoc_convert
#' @importFrom jsonlite fromJSON
#' @rdname run_analysis
run_internal <- function(rmd_filename, params_filename, output_directory = tempdir()){
  args = commandArgs(trailingOnly=TRUE)
  
  md_filename <- file.path(output_directory, "REPORT.md")
  pandoc_filename <- file.path(output_directory, "REPORT.html")
  params_ <- fromJSON(params_filename)
  params_$output_directory <- output_directory
  owd <- setwd(output_directory)
  
  result <- try({
    rmarkdown::render(rmd_filename, output_file=pandoc_filename, params=params_)
  })
  setwd(owd)
  if(inherits(result, "try-error")){
    stop(result)
  }
  return(pandoc_filename)
}  

#' @rdname run_analysis
#' @param open_result Open compiled report in new window
#' @importFrom utils browseURL
#' @export
run_locally <- function(rmd_filename, params_filename, open_result = TRUE){
  pandoc_filename <- run_internal(rmd_filename, params_filename)
  if(open_result){
    browseURL(pandoc_filename)
  }
}  


#' @rdname run_analysis
#' @export
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom rmarkdown yaml_front_matter
#' @importFrom httr POST add_headers
#' @importFrom utils browseURL
run_on_cluster <- function(rmd_filename, params_filename, open_result = TRUE){
  submit_url <-  sprintf("%s/submit_job_token/",getOption("tltools.tlapp.base.url"))
  yaml_header <- yaml_front_matter(rmd_filename)
  r_packages <- yaml_header$required_packages
  payload <- list(ghap_credentials = ghap_credentials(),
                  inputs = fromJSON(params_filename),
                  backend = "ghap",
                  code = paste(readLines(rmd_filename), collapse="\n"),
                  r_packages = r_packages)
  payload_json <- toJSON(payload, auto_unbox = TRUE)
  headers <- add_headers(Authorization=tlapp_token(),
                         `Content-Type`="application/json")
  response <- POST(submit_url, body=payload_json, headers)
  if(response$status_code!=200){
    stop("Something went wrong with run_on_cluster. Status Code:", response$status_code)
  }
  
  
  
  if(open_result){
    job_url <- content(response)$results_url
    browseURL(job_url)
  }
}  
