#identify grupo from value

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

