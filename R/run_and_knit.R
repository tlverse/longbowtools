#' run_and_knit
#' @export
#' @importFrom knitr knit
run_and_knit <- function(Rmd_file, tlparams, run_pandoc=FALSE){
  
  output_Rmd_file <- file.path(tlparams$output_dir, "REPORT.Rmd")
  file.copy(Rmd_file, output_Rmd_file)
  md_file <- file.path(tlparams$output_dir, "REPORT.md")
  
  # knitr is sensitive to working directory
  # see https://github.com/yihui/knitr/issues/913#issuecomment-66897715
  owd <- setwd(tlparams$output_dir)
  try({
    knit(output_Rmd_file,output=md_file)
    
    if(run_pandoc){
      pandoc(md_file)
    }
  })
  
  setwd(owd)
}