
#' Generate sample params object using defaults specified in Rmd file
#' 
#' Can also be used to create an inputs.json file that can then be modified with other paramter values
#' 
#' @param rmd_filename the analysis template to use
#' @param save_as the json filename to save the resulting params to
#' @return the params object
#' 
#' @export
#' @importFrom knitr knit_params
#' @importFrom jsonlite toJSON
params_from_rmd <- function(rmd_filename, save_as=NULL){
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
  
  
  
  if(!is.null(save_as)){
    params_json <- toJSON(params)
    writeLines(params_json, save_as)
  }
  
  
  return(params)  
}


#' Script helpers
#' 
#' Get relevant objects from script parameters
#' @param params_object the object to use, defaults to the global \code{params} object
#' @export
#' 
#' @importFrom stringr str_extract str_to_lower
#' @importFrom data.table setDT
#' @rdname script_helpers
get_tl_data <- function(params_object = NULL){
  if(is.null(params_object)){
    params_object <- get("params", envir=parent.frame())
  }
  uri <- params_object$data$uri
  
  #check if file exists before proceeding
  if(!file.exists(uri)){
    stop('File does not exist: ', uri)
  }
  
  extension <- str_to_lower(str_extract(uri,"\\.([^\\.]+)$"))
  
  if(extension==".csv"){
    data <- fread(uri)
  } else if(extension==".rds"){
    data <- readRDS(uri)
    setDT(data)
  } else if(extension==".rdata"){
    vars <- load(file(uri, "rb"))
    
    #use first object that's a data.frame
    dfs <- sapply(vars,function(var)is.data.frame(get(var)))
    if(length(which(dfs))==0){
      stop("rdata file does not contain a data frame")
    }
    var <- vars[which(dfs)][1]
    data <- get(var)
    setDT(data)
  } else {
    stop("unrecognized data file extension: ",extension)
  }
  
  return(data)
}

#' @rdname script_helpers
#' @export
get_tl_nodes <- function(params_object = NULL){
  if(is.null(params_object)){
    params_object <- get("params", envir=parent.frame())
  }
  
  nodes <- params_object$nodes
  
  data <- get_tl_data()
  missing_cols <- setdiff(unlist(nodes), colnames(data))
  if(length(missing_cols) > 0){
    stop('Column(s) missing from data: ', missing_cols)
  }
  
  #drop exclude list
  nodes$exclude <- NULL
  return(nodes)
}

#' @rdname script_helpers
#' @export
get_tl_params <- function(params_object = NULL){
  if(is.null(params_object)){
    params_object <- get("params", envir=parent.frame())
  }
  
  script_params <- params_object$script_params
  # in rstudio these get passed in from YAML not as their value, 
  # but as the whole object, so subset value if that's what we have
  values <- lapply(script_params, function(script_param){
    if(is.list(script_param)){
      return(script_param$value)
    } else {
      return(NULL)
    }
  })

  if(any(sapply(values,is.null))){
    return(script_params)
  } else {
    return(values)
  }
}