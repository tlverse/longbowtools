#' @rdname run_analysis
#' @export
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom rmarkdown yaml_front_matter
#' @importFrom httr POST add_headers
#' @importFrom utils browseURL
run_on_longbow <- function(rmd_filename, params_filename, open_result = TRUE, provision = TRUE){
  submit_url <-  sprintf("%s/submit_job_token/",getOption("longbowtools.longbow.base.url"))
  yaml_header <- yaml_front_matter(rmd_filename)
  r_packages <- yaml_header$required_packages
  payload <- list(ghap_credentials = cluster_credentials(),
                  inputs = fromJSON(params_filename),
                  backend = "ghap",
                  code = paste(readLines(rmd_filename), collapse="\n"),
                  r_packages = r_packages)
  
  payload$skip_provision <- !provision
  if(!provision){
    
  }
  payload_json <- toJSON(payload, auto_unbox = TRUE)
  headers <- add_headers(Authorization=longbow_token(),
                         `Content-Type`="application/json")
  response <- POST(submit_url, body=payload_json, headers)
  if(response$status_code!=200){
    stop("Something went wrong with run_on_longbow. Status Code:", response$status_code)
  }
  
  job_url <- content(response)$results_url
  job_id <- gsub("/","",gsub(".*/jobs/","",job_url))
  if(open_result){
    browseURL(job_url)
  }
  
  return(job_id)
}  

#' Publish template to longbow UI
#' @param rmd_filename the template to publish
#' @param open_result if \code{TRUE}, opens compiled report in browser
#' @export
publish_template <- function(rmd_filename, open_result = TRUE){
  submit_url <-  sprintf("%s/templates/",getOption("longbowtools.longbow.base.url"))
  payload <- paste(readLines(rmd_filename), collapse="\n")
  headers <- add_headers(Authorization=longbow_token(),
                         `Content-Type`="application/json")
  response <- POST(submit_url, body=payload, headers)
  if(response$status_code!=200){
    stop("Something went wrong with run_on_longbow. Status Code:", response$status_code)
  }
  
  if(open_result){
    url <- content(response)$url
    browseURL(url)
  }
  
}  

#' Longbow Job API
#' @param job_id the job_id of the job being requested. Returned by \code{\link{run_on_longbow}}
#' @export
get_job_logs <- function(job_id){
  log_url <-  sprintf("%s/jobs/%s/logs_token/",getOption("longbowtools.longbow.base.url"), job_id)
  headers <- add_headers(Authorization=longbow_token())
  response <- GET(log_url, headers)
  if(response$status_code!=200){
    stop("Something went wrong with getting the logs. Status Code:", response$status_code)
  }
  
  logs <- content(response,as="parsed")
  
  return(logs$logs)
}

#' @export
get_job_status <- function(job_id){
  status_url <-  sprintf("%s/jobs/%s/?format=json",getOption("longbowtools.longbow.base.url"), job_id)
  headers <- add_headers(Authorization=longbow_token())
    
  response <- GET(status_url, headers)
  resp_data <- content(response)
  if(length(resp_data$jobs)==0){
    statuses <- resp_data$status
  } else {
    # get vector of statuses for batch job
    statuses <- sapply(resp_data$jobs,`[[`,"status")
    job_ids <- sapply(resp_data$jobs,`[[`,"id")
    names(statuses) <- job_ids
  }

  return(statuses)
}

#' @importFrom utils download.file untar
#' @importFrom httr GET content
#' @export
get_job_output <- function(job_id, download_directory = tempdir()){
  download_url_url <-  sprintf("%s/jobs/%s/download_url_token/",getOption("longbowtools.longbow.base.url"), job_id)
  headers <- add_headers(Authorization=longbow_token())
  response <- GET(download_url_url, headers)
  if(response$status_code!=200){
    stop("Something went wrong with getting the downlad_url. Status Code:", response$status_code)
  }
  
  download_url <- content(response,as="parsed")
  destination_folder <- file.path(download_directory, sprintf("job_results_%s",job_id))
  
  result <- try({
  
    dest_file <- file.path(tempdir(),"output.tar.gz")
    suppressWarnings({download.file(download_url, dest_file, quiet=TRUE, method="curl")})
    files <- untar(dest_file, list = TRUE)
    untar(dest_file, exdir=download_directory)
    
    extracted_folder <- file.path(download_directory, files[[1]])
    
    file.rename(extracted_folder,destination_folder)
    
  }, silent = TRUE)
  
  if(inherits(result,"try-error")){
    # probably an error, create folder for logs
    message(sprintf("\njob:%s has no output, getting logs only",job_id))
    dir.create(destination_folder)
  }
  
  logs <- get_job_logs(job_id)
  logs_file <- file.path(destination_folder, "logs.txt")
  writeLines(logs, logs_file)
  
  return(destination_folder)
}

#' Longbow Job API
#' @param job_id the job_id of the job being requested. Returned by \code{\link{run_on_longbow}}
#' @export
force_job_finish <- function(job_id){
  finish_url <-  sprintf("%s/jobs/%s/finish/",getOption("longbowtools.longbow.base.url"), job_id)
  headers <- add_headers(Authorization=longbow_token())
  response <- POST(finish_url, headers, body="{}")
  if(response$status_code!=200){
    stop("Something went wrong with getting the logs. Status Code:", response$status_code)
  }
  
  logs <- content(response,as="parsed")
  
  return(logs$logs)
}