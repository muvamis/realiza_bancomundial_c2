define_dir_data <- function(){
  
  sistema <- Sys.info()['sysname']
  
  if(sistema == "Windows"){
    
    dir_data <- "C:/repositaries/3.MUVA/realiza/data"
  } else {
    
    dir_data <- '/srv/shiny-server/2023/realiza/data'
    
  }
  
  dir_data
}