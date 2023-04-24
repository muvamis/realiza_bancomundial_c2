identify_mode <- function(mode){
  
  if(str_detect(mode, "tabela")){
    
    mode = "table"
    
  } else if(str_detect(mode, "chart")){
    
    mode = "chart"
  } 
  
  return(mode)
  
}