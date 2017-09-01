#' Main TLtools class
#' todo: write docs
#' @export
#' @importFrom jsonlite read_json
ScriptParams <- R6Class(classname = "ScriptParams",
                   portable = TRUE,
                   class = TRUE,
                   public = list(
                     initialize = function(input_file, lazy_load_data = FALSE) {
                       
                       private$.input_file <- input_file
                       parsed <- jsonlite::read_json(input_file)
                       
                       if(is.null(parsed$data)){
                         stop("Input is missing data specification")
                       } else {
                         private$.data_spec=parsed$data
                         
                         if(!lazy_load_data){
                           private$.load()
                         }
                       }
                       
                       if(is.null(parsed$params)){
                         stop("Input is missing params specification")
                       } else {
                         private$.params <- parsed$params
                       }
                       
                       if(is.null(parsed$output_dir)){
                         stop("Input is missing output directory")
                       } else {
                         private$.output_dir <- parsed$output_dir
                         if(!dir.exists(parsed$output_dir)){
                           message("Output dir: ", parsed$output_dir, " does not exist, attempting to create")
                           dir.create(parsed$output_dir,recursive = TRUE, showWarnings = TRUE)
                         }
                       }
                     }
                   ),
                   active = list(
                     input_file=function(){
                       return(private$.input_file)
                     },
                     data=function(){
                       if(is.null(private$.data)){
                         private$.load()
                       }
                       
                       return(private$.data)
                     },
                     data_nodes=function(){
                       return(private$.data_spec$nodes)
                     },
                     data_spec=function(){
                       return(private$.data_spec)
                     },
                     params=function(){
                       return(private$.params)
                     },
                     output_dir=function(){
                       return(private$.output_dir)
                     }
                   ),
                   private = list(
                     .input_file = NULL,
                     .output_dir = NULL,
                     .params = NULL,
                     .data_spec = NULL,
                     .data = NULL,
                     .args = args,
                     .load = function(){
                       
                       data_spec <- private$.data_spec
                       message("Loading data from ", data_spec$uri)
                       if(data_spec$type=='csv'){
                         private$.data = data.table::fread(data_spec$uri)
                       } else {
                         stop("Unsupported data type: ",data_spec$type)
                       }
                     }
                   )
)