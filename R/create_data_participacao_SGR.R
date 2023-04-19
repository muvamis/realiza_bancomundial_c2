

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


create_data_participacao_SGR <- function(db_sgr,db_emprendedoras, by){


  message(by)
  
  
  if(by == "Seu todo"){
    
    library(dplyr)
    library(ggplot2)
    

    
    presencas_count %>% filter(grupo_accronym %in% "SGR") %>%
      ggplot() +
      aes(x = actividade, y = n, fill = grupo_accronym) +
      geom_col() +
      scale_fill_hue(direction = 1) +
      theme_minimal()
     
    
    
    
    #count total women in listas
    totais <- create_data_totais(dplyr::filter(db_emprendedoras, grupo_accronym != "FNM"), by)
    
    #count women in different ranges of progress
    data_comp <- db_sgr %>%
      count_mulheres(actividade) %>%
      mutate(target = "Seu todo") %>%
      left_join(totais, by = "target") %>%
      estimate_prop() 
    
    
    
  } else if(by == "Por Cidade") {
    
    totais <- create_data_totais(filter(db_emprendedoras, grupo_accronym != "FNM"), by)
    
    data_comp <- db_sgr %>%
      count_mulheres(Cidade,actividade) %>%
      rename(target = Cidade) %>%
      left_join(totais,  by = "target") %>%
      estimate_prop() 
    
  } else if(by == "Por Abordagem"){
    
    totais <- create_data_totais(filter(db_emprendedoras, grupo_accronym != "FNM"), by)
    
    data_comp <- db_sgr %>%
      count_mulheres(Abordagem,actividade) %>%
      rename(target = Abordagem) %>%
      count_mulheres(target, actividade) %>%
       left_join(totais, by = "target") %>%
      estimate_prop() 
    
    
  }
  
 
  
  data_comp %>%
    mutate(actividade = factor(actividade,
                               levels = c("1.1",
                                          "Introducao a sessao de parceiros",
                                          "1.2", "1.3",
                                          "2.1", "2.2", "2.3",
                                          "Sessao intercalar de parceiros",  
                                          "3.1", "3.2", "3.3",
                                          "Sessao de encerramento de parceiros",
                                          "Sessões de coaching"
                                          ),
                               ordered = T
                               
                               ))
  
  
}
