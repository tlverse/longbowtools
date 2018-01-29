#' Provide ghap credentials for use with run_on_cluster
#' 
#' @param username your GHAP username
#' @param password your GHAP password
#' @param ip the ip of your GHAP node
#' @param tlapp_token an api token for the tlapp
#' @export
configure_ghap <- function(username, password, ip, tlapp_token){
  options(tltools.ghap.username = username, 
          tltools.ghap.password = password,
          tltools.ghap.ip = ip,
          tltools.tlapp.token = tlapp_token)
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