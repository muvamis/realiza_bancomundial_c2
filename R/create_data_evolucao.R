#'Count presencas over period of time
#' based on the user inputs
install_data_packages()

#'creates data of total presencas by period
#'@param presencas data base of presencas created by define_var_periodo
#'@param by input$by in panels participacao

create_data_evolucao <- function(presencas, by){
  
  
  if(by == "Seu todo"){
    
    db <- presencas %>%
      group_by(Emprendedora, periodo) %>%
      slice(1) %>%
      ungroup() %>%
      group_by(periodo) %>%
      summarise(presentes = n(),
                .groups = 'drop') %>%
      mutate(target = "Seu todo")
    
    
  } else if (by == "Por Cidade"){
    
    db <- presencas %>%
      group_by(Emprendedora, Cidade, periodo) %>%
      slice(1) %>%
      ungroup() %>%
      group_by(Cidade, periodo) %>%
      summarise(presentes = n(),
                .groups = 'drop') %>%
      rename(target = Cidade)
    
    
    
    
  } else if (by == "Por Abordagem"){
    
    db <- presencas %>%
      group_by(Emprendedora,Abordagem , periodo) %>%
      slice(1) %>%
      ungroup() %>%
      group_by(Abordagem, periodo) %>%
      summarise(presentes = n(),
                .groups = 'drop') %>%
      rename(target = Abordagem)
    
    
    
    
  } else if (by == "Por Cidade e Abordagem"){
    
    db <- presencas %>%
      group_by(Emprendedora, Abordagem, Cidade, periodo) %>%
      slice(1) %>%
      ungroup() %>%
      group_by(Abordagem, Cidade, periodo) %>%
      summarise(presentes = n(),
                .groups = 'drop') %>%
      rename(target = Abordagem,
             facet = Cidade)
    
    
  }
  
  db
  
  
}


