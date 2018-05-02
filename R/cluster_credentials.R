#' Provide cluster credentials for use with run_on_cluster
#' 
#' @param json_file the configuration file
#' @importFrom jsonlite fromJSON
#' @export
configure_cluster <- function(json_file="~/cluster_credentials.json"){
  credentials = fromJSON(json_file)
  with(credentials,
       options(longbowtools.cluster.username = username, 
               longbowtools.cluster.password = password,
               longbowtools.cluster.ip = ip,
               longbowtools.longbow.token = longbow_token))
}

#' Get GHAP credentials provided to configure_cluster
#' @export
cluster_credentials <- function(){
  cluster_credentials <- list(username = getOption("longbowtools.cluster.username"),
                           password = getOption("longbowtools.cluster.password"),
                           ip = getOption("longbowtools.cluster.ip"))
  if(any(sapply(cluster_credentials, is.null))){
    stop("Set cluster credentials using configure_cluster()")
  }
  
  return(cluster_credentials)
}

#' Get longbow token provided to configure_cluster
#' @export
longbow_token <- function(){
  token <- getOption("longbowtools.longbow.token")
  if(is.null(token)){
    stop("Set longbow token using configure_cluster()")
  }
  
  return(token)
}