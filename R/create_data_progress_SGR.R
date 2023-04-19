

#'creates data of progress made by women in FNM
#'

#utils =========================================================================



#Function to count mulheres by certain groups
count_mulheres <- function(.data, ...){
  
  .data %>%
    group_by(...) %>%
    summarise(mulheres = sum(mulheres),
              .groups = 'drop')
}

#estimate proportion of progress by WB and interesadas
estimate_prop <- function(.data){
  .data %>%
    mutate(prop_int = mulheres / interesadas,
           prop_wb = mulheres / total)
}



#=================================================================================


create_data_progress_SGR <- function(db_sgr,db_emprendedoras, by){


  message(by)
  
  
  if(by == "Seu todo"){
    
    #count total women in listas
    totais <- create_data_totais(filter(db_emprendedoras, grupo_accronym != "FNM"), by)
    
    #count women in different ranges of progress
    data_comp <- db_sgr %>%
      count_mulheres(completos) %>%
      mutate(target = "Seu todo") %>%
      left_join(totais, by = "target") %>%
      estimate_prop() 
    
    
    
  } else if(by == "Por Cidade") {
    
    totais <- create_data_totais(filter(db_emprendedoras, grupo_accronym != "FNM"), by)
    
    data_comp <- db_sgr %>%
      count_mulheres(Cidade,completos) %>%
      rename(target = Cidade) %>%
      left_join(totais,  by = "target") %>%
      estimate_prop() 
    
  } else if(by == "Por Abordagem"){
    
    totais <- create_data_totais(filter(db_emprendedoras, grupo_accronym != "FNM"), by)
    
    data_comp <- db_sgr %>%
      count_mulheres(Abordagem,completos) %>%
      rename(target = Abordagem) %>%
      count_mulheres(target, completos) %>%
       left_join(totais, by = "target") %>%
      estimate_prop() 
    
    
  }
  
  
  data_comp 
  
  
}
