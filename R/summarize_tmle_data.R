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

roles_from_node_list <- function(variables, node_list){
  all_nodes <- unlist(node_list)
  all_roles <- rep(names(node_list), sapply(node_list, length))
  roles <- all_roles[match(variables, all_nodes)]
  
  return(roles)
}

#' @export
#' 
#' @importFrom skimr skim
summarize_tmle_data <- function(data, node_list){
  tmle_data <- data[, unlist(node_list), with=FALSE]
  variable_types <- infer_variable_types(tmle_data)
  
    
  # include extra role variable in output
  skim_list <- skim_to_list(tmle_data, variable_types)
  role_order <- names(node_list)
  add_roles_to_skim_table <- function(skim_table){
    roles <- roles_from_node_list(skim_table$variable, node_list)
    skim_table <- cbind(role = roles, skim_table)
    skim_table[order(match(skim_table$role, role_order), skim_table$variable), ]
  }
  
  skim_list <- lapply(skim_list, add_roles_to_skim_table)
  
  return(skim_list)
}
