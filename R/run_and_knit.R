#' Run script with parameters as specified and generate markdown output
#' @param Rmd_file the script to run
#' @param tlparams a ScriptParams object specifying the relevant options
#' @param run_pandoc boolean, indicating whether to run pandoc on the markdown output
#' @export
#' @importFrom knitr knit
run_and_knit <- function(Rmd_file, tlparams, run_pandoc=FALSE){
  Rmd_file_name <- "REPORT.Rmd"
  md_file_name <- "REPORT.md"
  output_Rmd_path <- file.path(tlparams$output_dir, Rmd_file_name)
  file.copy(Rmd_file, output_Rmd_path, overwrite = TRUE)
  md_file <- file.path(tlparams$output_dir, "md_file_name")
  
  # knitr is sensitive to working directory
  # see https://github.com/yihui/knitr/issues/913#issuecomment-66897715
  owd <- setwd(tlparams$output_dir)
  try({
    knit(Rmd_file_name,output=md_file_name)
    
    if(run_pandoc){
      pandoc(md_file_name)
    }
  })
  
  setwd(owd)
}