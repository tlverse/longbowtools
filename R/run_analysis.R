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
#' @param open_result Open compiled report in new window
#' @importFrom utils browseURL
#' @export
run_locally <- function(rmd_filename, params_filename, output_directory=tempdir(), open_result = TRUE){
  pandoc_filename <- run_internal(rmd_filename, params_filename, output_directory=output_directory)
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
  
  return(job_url)
}  


#' @export
publish_template <- function(rmd_filename, open_result = TRUE){
  submit_url <-  sprintf("%s/templates/",getOption("tltools.tlapp.base.url"))
  payload <- paste(readLines(rmd_filename), collapse="\n")
  headers <- add_headers(Authorization=tlapp_token(),
                         `Content-Type`="application/json")
  response <- POST(submit_url, body=payload, headers)
  if(response$status_code!=200){
    stop("Something went wrong with run_on_cluster. Status Code:", response$status_code)
  }
  
  if(open_result){
    url <- content(response)$url
    browseURL(url)
  }
  
}  


#' @export
get_job_logs <- function(job_id){
  log_url <-  sprintf("%s/jobs/%s/logs_token/",getOption("tltools.tlapp.base.url"), job_id)
  headers <- add_headers(Authorization=tlapp_token())
  response <- GET(log_url, headers)
  if(response$status_code!=200){
    stop("Something went wrong with getting the logs. Status Code:", response$status_code)
  }
  
  logs <- content(response,as="parsed")
  
  return(logs$logs)
}

#' @export
get_job_status <- function(job_id){

  status_url <-  sprintf("%s/jobs/%s/status_token/",getOption("tltools.tlapp.base.url"), job_id)
  headers <- add_headers(Authorization=tlapp_token())
  response <- GET(status_url, headers)
  if(response$status_code!=200){
    stop("Something went wrong with getting the logs. Status Code:", response$status_code)
  }
  
  status <- content(response,as="parsed")
  return(status)
}

#' @importFrom utils download.file untar
#' importFrom httr GET content
#' @export
get_job_output <- function(job_id, download_directory = tempdir()){
  download_url_url <-  sprintf("%s/jobs/%s/download_url_token/",getOption("tltools.tlapp.base.url"), job_id)
  headers <- add_headers(Authorization=tlapp_token())
  response <- GET(download_url_url, headers)
  if(response$status_code!=200){
    stop("Something went wrong with getting the downlad_url. Status Code:", response$status_code)
  }
  
  download_url <- content(response,as="parsed")
  
  dest_file <- file.path(tempdir(),"output.tar.gz")
  download.file(download_url, dest_file, quiet=TRUE)
  files <- untar(dest_file, list = TRUE)
  untar(dest_file, exdir=download_directory)
  
  extracted_folder <- file.path(download_directory, files[[1]])
  destination_folder <- file.path(download_directory, sprintf("job_results_%s",job_id))
  file.rename(extracted_folder,destination_folder)
  
  return(destination_folder)
}