#'creates data of total emprendedoras to plot in participacao
#'@param db_emprendedoras data base of emprendedoras (look up)
#'@param by input$by in panels participacao

create_data_totais <- function(db_emprendedoras, by){
  
  
  if(by == "Seu todo"){
    
    db <- db_emprendedoras %>%
      summarise(total = n(),
                interesadas = sum(status_realiza == "CONFIRMADA")) %>%
      mutate(target = "Seu todo")
    
  } else if (by == "Por Cidade"){
    
    db <- db_emprendedoras %>%
      group_by(Cidade) %>%
      summarise(total = n(),
                interesadas = sum(status_realiza == "CONFIRMADA"),
                .groups = 'drop') %>%
      rename(target = Cidade)
    
    
    
    
  } else if (by == "Por Abordagem"){
    
    db <- db_emprendedoras %>%
      group_by(grupo_accronym) %>%
      summarise(total = n(),
                interesadas = sum(status_realiza == "CONFIRMADA"),
                .groups = 'drop') %>%
      rename(target = grupo_accronym)
    
    
    
    
  } else if (by == "Por Cidade e Abordagem"){
    
    db <- db_emprendedoras %>%
      group_by(Cidade, grupo_accronym) %>%
      summarise(total = n(),
                interesadas = sum(status_realiza == "CONFIRMADA"),
                .groups = 'drop') %>%
      rename(target =  grupo_accronym,
             facet = Cidade)
    
    
  }
  
  db
  
  
}
