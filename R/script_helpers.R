#' Generate sample inputs.json object using defaults specified in Rmd file
#' 
#' can be used to create an inputs.json file that can then be modified with other paramter values
#' 
#' @param rmd_filename the analysis template to use
#' @param save_as the filename to save the resulting inputs.json to
#' @return the raw json text
#' 
#' @export
#' @importFrom knitr knit_params
#' @importFrom jsonlite toJSON
inputs_json_from_rmd <- function(rmd_filename, save_as=NULL){
  lines <- readLines(rmd_filename, warn = FALSE)
  params_raw <- knit_params(lines)
  
  # pull out defaults
  params <- list()
  for (param in params_raw) {
    params[[param$name]] <- param$value
  }
  
  # pull out default script params
  script_params_raw <- params$script_params
  script_params <- lapply(script_params_raw, `[[`, "value")
  names(script_params) <- names(script_params_raw)
  params$script_params <- script_params
  
  inputs_json <- toJSON(params)
  
  if(!is.null(save_as)){
    writeLines(inputs_json, save_as)
  }
  
  
  return(inputs_json)  
}

#' Script helpers
#' 
#' Get relevant objects from script parameters
#' @param params_object the object to use, defaults to the global \code{params} object
#' @export
#' 
#' @rdname script_helpers
get_tl_data <- function(params_object = NULL){
  if(is.null(params_object)){
    params_object <- get("params", envir=parent.frame())
  }
  dataset_uri <- params_object$data$dataset_uri
  fread(dataset_uri)
}

#' @rdname script_helpers
#' @export
get_tl_nodes <- function(params_object = NULL){
  if(is.null(params_object)){
    params_object <- get("params", envir=parent.frame())
  }
  
  params_object$nodes
}

#' @rdname script_helpers
#' @export
get_tl_params <- function(params_object = NULL){
  if(is.null(params_object)){
    params_object <- get("params", envir=parent.frame())
  }
  
  params_object$script_params
}