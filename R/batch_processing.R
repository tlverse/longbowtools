#todo: define batch object to manage some of this
#' @importFrom progress progress_bar
#' @export
submit_batch <- function(rmd_filename, inputs_folder = "inputs", results_folder = "results", provision = TRUE){
  	inputs_files <- dir(inputs_folder, full.names = TRUE)
  	
  	cat(sprintf("Submitting jobs..."))
  	# submit jobs
  	job_ids <- sapply(inputs_files, function(inputs_file){
  	  run_on_longbow(rmd_filename, inputs_file, open_result = FALSE, provision = provision)  
  	})
  	save(job_ids, file="job_ids.rdata")
  	
  	
  	cat(sprintf("\t%s jobs submitted\n",length(job_ids)))
  
  	return(job_ids) 	
}

#' @export
wait_for_batch <- function(job_ids){
  # wait for jobs to finish
  jobs_pending = job_ids
  
  cat(sprintf("Running jobs...\n",length(job_ids))) 	
  pb <- progress_bar$new(format="[:bar] :percent", total=length(job_ids), clear=TRUE)
  pb$tick(0)
  while(length(jobs_pending)>0){
    pending_statuses <- c("submitted", "running")
    job_statuses <- get_job_statuses(job_ids)
    # job_table <- data.table(job_ids = job_ids, inputs_file=inputs_files, status=job_statuses)
    jobs_pending <- job_ids[job_statuses%in%pending_statuses]
    fraction_remaining <- (length(job_ids)-length(jobs_pending))/length(job_ids)
    pb$update(fraction_remaining)
    
    if(length(jobs_pending)>0){
      Sys.sleep(5)
    }
  }
}

#' @export
get_batch_results <- function(job_ids, results_folder="results"){
  cat(sprintf("Downloading results...\n"))
  job_statuses <- get_job_statuses(job_ids)
  
  # find jobs where export failed and reattempt
  # success_job_ids <- job_ids[which(job_statuses%in%c("success","executed"))]
  # if(length(success_job_ids)>0){
  #   lapply(success_job_ids, force_job_finish)
    
  #   # if jobs are still stuck at "success", report that
  #   job_statuses <- get_job_statuses(job_ids)
  #   success_job_ids <- job_ids[which(job_statuses%in%c("success","executed"))]
    
  #   if(length(success_job_ids)>0){
  #    message("Export failed for the following jobs: ", paste0(success_job_ids, collapse=", ")) 
  #   }
  # }
  
  viewable_job_ids <- job_ids[which(job_statuses==c("success"))]
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
