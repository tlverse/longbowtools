#todo: define batch object to manage some of this
#' @importFrom progress progress_bar
process_batch <- function(rmd_filename, inputs_folder="inputs", results_folder="results"){
  	inputs_files <- dir(inputs_folder, full.names = TRUE)
  	
  	cat(sprintf("Submitting jobs..."))
  	# submit jobs
  	job_ids <- sapply(inputs_files, function(inputs_file){
  	  run_on_longbow(rmd_filename, inputs_file, open_result = FALSE)  
  	})
  	save(job_ids, file="job_ids.rdata")
  	
  	
  	cat(sprintf("\t%s jobs submitted\n",length(job_ids)))
  	
  	# wait for jobs to finish
  	jobs_pending = job_ids
    
  	cat(sprintf("Running jobs...\n",length(job_ids))) 	
  	pb <- progress_bar$new(format="[:bar] :percent", total=length(job_ids), clear=TRUE)
  	pb$tick(0)
  	while(length(jobs_pending)>0){
  	  pending_statuses <- c("submitted", "running")
  	  job_statuses <- sapply(job_ids, get_job_status)
  	  # job_table <- data.table(job_ids = job_ids, inputs_file=inputs_files, status=job_statuses)
  	  jobs_pending <- job_ids[job_statuses%in%pending_statuses]
  	  fraction_remaining <- (length(job_ids)-length(jobs_pending))/length(job_ids)
  	  pb$update(fraction_remaining)
  	  
  	  if(length(jobs_pending)>0){
  	    Sys.sleep(5)
  	  }
  	}
  	
  	cat(sprintf("Downloading results...\n"))
  	success_job_ids <- job_ids[which(job_statuses=="viewable")]
  	pb <- progress_bar$new(format="[:bar] :percent", total=length(success_job_ids), clear=TRUE)
  	pb$tick(0)
  	for(job_id in success_job_ids){
  	  get_job_output(job_id, results_folder)
  	  pb$tick()
  	}
  	while(length(jobs_pending)>0){
  	  pending_statuses <- c("submitted", "running")
  	  job_statuses <- sapply(job_ids, get_job_status)
  	  # job_table <- data.table(job_ids = job_ids, inputs_file=inputs_files, status=job_statuses)
  	  jobs_pending <- job_ids[job_statuses%in%pending_statuses]
  	  fraction_remaining <- (length(job_ids)-length(jobs_pending))/length(job_ids)
  	  pb$update(fraction_remaining)
  	  
  	  if(length(jobs_pending)>0){
  	    Sys.sleep(5)
  	  }
  	}
  	
}

load_from_jobs <- function(jobs_folder){
  
}