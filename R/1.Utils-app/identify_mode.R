#'@description the naming of the ids include the word tabela or chart. This function
#'identifies which module the id refers to.

identify_mode <- function(mode){
  
  if(str_detect(mode, "tabela")){
    
    mode = "table"
    
  } else if(str_detect(mode, "chart")){
    
    mode = "chart"
  } 
  
  return(mode)
  
}