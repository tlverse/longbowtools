#' Provide ghap credentials for use with run_on_cluster
#' 
#' @param json_file the configuration file
#' @importFrom jsonlite fromJSON
#' @export
configure_ghap <- function(json_file="~/ghap.json"){
  credentials = fromJSON(json_file)
  with(credentials,
       options(tltools.ghap.username = username, 
               tltools.ghap.password = password,
               tltools.ghap.ip = ip,
               tltools.tlapp.token = tlapp_token))
}

#' Get GHAP credentials provided to configure_ghap
#' @export
ghap_credentials <- function(){
  ghap_credentials <- list(username = getOption("tltools.ghap.username"),
                           password = getOption("tltools.ghap.password"),
                           ip = getOption("tltools.ghap.ip"))
  if(any(sapply(ghap_credentials, is.null))){
    stop("Set ghap credentials using configure_ghap()")
  }
  
  return(ghap_credentials)
}

#' Get tlapp token provided to configure_ghap
#' @export
tlapp_token <- function(){
  tlapp_token <- getOption("tltools.tlapp.token")
  if(is.null(tlapp_token)){
    stop("Set tlapp token using configure_ghap()")
  }
  
  return(tlapp_token)
}