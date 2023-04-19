#run all modules within folders in R

#read all folders
all_modules <- list.dirs("R", recursive = T, full.names = T)


cli::cli_alert_info("Reading these scripts")
read_all <- lapply(all_modules, function(module){
 
  
  if(module != "R") {
    
    module_scripts <- list.files(module,pattern = ".R", full.names = T)
    
    
    
    for(script in module_scripts) {
      
      message(script)
      
      source(script, encoding = "UTF-8")


    }
    
  }
  
  
  
})

