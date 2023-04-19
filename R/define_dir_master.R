
#' define master directory
#' @return the path to the directory where the data is stored (in /realiza)

define_dir_master <- function(){
  
  sistema <- Sys.info()["sysname"]
  
  if(sistema == "Windows"){
    
    dir_master <- file.path(dirname(getwd()), "realiza")
    
  } else {
    
    dir_master <- "/srv/shiny-server/realiza"
  }
  
  
  dir_master
}

define_dir_master()
