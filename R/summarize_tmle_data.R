#' @export
#' @importFrom skimr skim
summarize_tmle_data <- function(tmle_task){
  tmle_data <- tmle_task$data
  c1 <- skim(tmle_data)
  # variable_names <- names(tmle_data)
  # t1 <- CreateTableOne(vars = variable_names, data = tmle_data)
  # c1 <- print(t1, printToggle = FALSE, noSpaces = TRUE, missing = TRUE)
  # 
  # tmle_nodes <- tmle_task$tmle_nodes
  # variable_map <- lapply(tmle_nodes, `[[`, "variables")
  # variable_roles <- rep(names(variable_map), sapply(variable_map,length))
  # 
  # #todo use variable types when summarizing (continuous vs cat)
  # node_types <- lapply(tmle_nodes, `[[`, "variable_type")
  # variable_types <- node_types[match(variable_roles, names(variable_map))]
  # 
  # pmissing <- apply(tmle_data, 2, function(x)mean(is.na(x)))
  # tab <- data.frame(role=variable_roles, pmissing=pmissing, summary=c1[-1,1])
  # rownames(tab) <- variable_names
  return(c1)
}
