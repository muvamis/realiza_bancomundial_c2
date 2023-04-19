#To count number of emprendedoras
#returns a dataset with number of emprendedoras by defined attributes


count_emprendedoras <- function(emprendedoras_db, 
                                grupo = c("SGR", "FNM"),
                                agrupar_por = c("Seu todo", "Por cidade")
){
  
  data_function <- emprendedoras_db %>%
    filter(grupo_accronym %in% grupo)
  
  
  
  if(agrupar_por == "Seu todo"){
    
    data_return = tibble(total = nrow(data_function))
    
  } else {
    
    if(agrupar_por == "Por cidade"){
      
      grupo_by = c('Cidade')
    }
    
    data_return <- data_function %>%
      group_by_at(grupo_by) %>%
      summarise(total = n())
    
  }
  
  
  
  return(data_return)
  
}
