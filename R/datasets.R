#' configure dataset for use with longbow
#' 
#' @param dataset_path path to the datset (for now csv files only)
#' @importFrom jsonlite toJSON
#' @importFrom data.table fread
#' @export
configure_dataset <- function(dataset_path){
  data <- fread(dataset_path)
  names <- names(data)
  dataset_dir <- dirname(dataset_path)
  dataset_file <- basename(dataset_path)
  #todo: this includes username, which breaks marc's cluster dataset detection here: https://github.com/BerkeleyBiostats/longbow/blob/master/core/tasks.py#L122
  repo_url <- system(sprintf("cd %s \n git remote get-url origin", dataset_dir), intern=TRUE)
  path_in_repo <- system(sprintf("cd %s \n git ls-tree --full-name --name-only HEAD %s", dataset_dir, dataset_file), intern=TRUE)
  names_json <- toJSON(list(names=names))
  cat(sprintf("URL: %s \n\n", repo_url))
  cat(sprintf("Variables: %s \n\n", names_json))
  cat(sprintf("Repository path: %s \n\n", path_in_repo))
}