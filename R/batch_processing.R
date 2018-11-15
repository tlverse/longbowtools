#' @export
wait_for_batch <- function(batch_id){
  # wait for jobs to finish
  job_statuses <- get_job_status(batch_id)
  jobs_pending <- job_statuses
  cat(sprintf("Running jobs...\n",length(job_statuses))) 	
  pb <- progress_bar$new(format="[:bar] :percent", total=length(job_statuses), clear=TRUE)
  pb$tick(0)
  while(length(jobs_pending)>0){
    finished_statuses <- c("success", "error")
    job_statuses <- get_job_status(batch_id)
    jobs_pending <- job_statuses[!(job_statuses%in%finished_statuses)]
    fraction_remaining <- (length(job_statuses)-length(jobs_pending))/length(job_statuses)
    pb$update(fraction_remaining)
    
    if(length(jobs_pending)>0){
      Sys.sleep(5)
    }
  }
}

#' @export
get_batch_results <- function(batch_id, results_folder="results"){
  # delete and recreate results to clear out old results
  if(dir.exists(results_folder)){
    unlink(results_folder, recursive = TRUE)
  }
  dir.create(results_folder)
  
  cat(sprintf("Downloading results...\n"))
  job_statuses <- get_job_status(batch_id)
  job_ids <- names(job_statuses)
  finished_statuses <- c("success", "error")
  viewable_job_ids <- job_ids[which(job_statuses%in%finished_statuses)]
  pb <- progress_bar$new(format="[:bar] :percent", total=length(viewable_job_ids), clear=TRUE)
  pb$tick(0)
  for(job_id in viewable_job_ids){
    get_job_output(job_id, results_folder)
    pb$tick()
  }
}

#' @export
load_batch_results <- function(results_file, results_folder="results"){
  all_results_folders <- dir(results_folder, full.names = TRUE , recursive = FALSE)
  results_files <- file.path(all_results_folders, results_file)
  one_results_file <- results_files[[1]]
  
  all_results <- lapply(results_files, function(one_results_file){
    if(file.exists(one_results_file)){
      obj_names <- load(one_results_file)
      return(get(obj_names[[1]]))  
    } else {
      warning("Expected results file ",one_results_file, " does not exist")
      return(NULL)
    }
  })
  
  results <- rbindlist(all_results, fill=TRUE)
}
