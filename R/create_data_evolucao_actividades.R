#'Count presencas actividades over time
#' based on the user inputs

#'creates data of total presencas by period
#'@param presencas data base of presencas created by define_var_periodo
#'@param by input$by in panels participacao

create_data_evolucao_actividades <- function(presencas, by){
  
  
  if(by == "Seu todo"){
    
    db <- presencas %>%
      group_by(actividade_label,periodo) %>%
      summarise(taxa = mean(presente),
                esperadas = n(),
                presente = sum(presente),
                .groups = 'drop')
    
    
  } else if (by == "Por Cidade"){
    
    db <- presencas %>%
      group_by(Cidade, actividade_label, periodo) %>%
      summarise(taxa = mean(presente),
                esperadas = n(),
                presente = sum(presente),
                .groups = 'drop') %>%
      rename(facet = Cidade)
    
    
    
    
  } else if (by == "Por Abordagem"){
    
    db <-  presencas %>%
      group_by(Abordagem, actividade_label, periodo) %>%
      summarise(taxa = mean(presente),
                esperadas = n(),
                presente = sum(presente),
                .groups = 'drop') %>%
      rename(facet = Abordagem)
    
    
    
  }   

  db
  
  
}
