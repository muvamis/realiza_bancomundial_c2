#
#' @description To count number of emprendedoras registered in programme
#' @param emprendedoras_db look up table of emprendedoras emprendedoras.rds
#' @param grupo grupo para contar c("SGR", "FNM", "FNM + SGR")
#' @param agrupar_por Variable para agrupar o numero de emprendedoras
#' @return a dataset with number of emprendedoras (total) by defined attributes



count_emprendedoras <- function(emprendedoras_db, 
                                grupo = c("SGR", "FNM"),
                                agrupar_por = c("Seu todo", "Por cidade")
){
  
  #filter the data based on the parameter grupo given by the user
  data_function <- emprendedoras_db %>%
    filter(grupo_accronym %in% grupo)
  
  
  
  #grouped data based on user paramenter agrupar_por
  if(agrupar_por == "Seu todo"){
    
    data_return = tibble(total = nrow(data_function))
    
  } else {
    
    if(agrupar_por == "Por cidade"){
      
      grupo_by = c('Cidade')
    }
    
    #count the number of emprendedoras based on user paramenters
    data_return <- data_function %>%
      group_by_at(grupo_by) %>%
      summarise(total = n())
    
  }
  
  
  
  return(data_return)
  
}
