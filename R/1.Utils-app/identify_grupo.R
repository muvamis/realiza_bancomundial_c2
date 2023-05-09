#'@description the naming of the ids of the modules always include the name of
#'the grupo. Thus, to avoid the use of if statements, this function detects the 
#'group and returns the name of the group used in the data.


identify_grupo <- function(tab){
  
  if(str_detect(tab, "cresca")){
    
    grupo = "SGR"
  } else if(str_detect(tab, "movimenta")){
    
    grupo = "SGR + FNM"
  } else if(str_detect(tab, "conecta")){
    
    grupo = "FNM"
  }
  
  return(grupo)
  
}

