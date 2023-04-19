#' Create data for tab ESSA SEMANA
#'@param db_this_week reactive data of the selected week
#'@param by variables to group by
#'@param todos whether the summarise should be for all observatins
#'@param double_group whether the grouping variables are two, in this case, the 
#'attribute shuld be a character vector


create_data_week <- function(db_this_week,
                             by,
                             todos = F,
                             double_group = F,
                             
                             ...
) {
  
  
  
  
  #seu todo
  if(todos){
    db <- db_this_week %>%
      group_by(Emprendedora) %>%
      slice(1) %>%
      ungroup() %>%
      summarise(value = n(),
                target = "Seu todo")
    
  }
  
  if(!todos & !double_group){
    
    db <- db_this_week %>%
      group_by(Emprendedora, {{by}}) %>%
      slice(1) %>%
      ungroup() %>%
      group_by(target ={{by}}) %>%
      summarise(value = n(),
                .groups = "drop"
      )
    
  }
  
  if(double_group){
    db <- db_this_week %>%
      group_by_at(c(by, "Emprendedora")) %>%
      slice(1) %>%
      ungroup() %>%
      group_by_at(by) %>%
      summarise(value = n(),
                .groups = 'drop')
    
    
  }
  
  return(db)
  
  
}
