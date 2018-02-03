n_unique_observed <- function(x){
  uni <- unique(x)
  return(length(uni[!is.na(uni)]))
}

infer_variable_types <- function(data){
  level_counts <- sapply(data, n_unique_observed)    
  native_class <- sapply(data, data.class)    
  variable_types <- ifelse(native_class=="factor","factor",
                           ifelse(level_counts<10, "factor", "numeric"))
}


#' @export
#' 
#' @importFrom skimr skim
summarize_tmle_data <- function(data, node_list){
  tmle_data <- data[, unlist(node_list), with=FALSE]
  variable_types <- infer_variable_types(tmle_data)
  c1 <- skim(tmle_data, variable_types)

  return(c1)
}
