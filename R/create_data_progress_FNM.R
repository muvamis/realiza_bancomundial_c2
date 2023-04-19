

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


#' estimates the number of emprendedoras that have not participated in the activity
#' group by what you want to count for
artifitial_not_attended <- function(.data, cats, ...){
  
  .data %>%
    group_by(...)  %>%
    summarise(prop_int = 1 - sum(prop_int),
              prop_wb = 1 - sum(prop_wb),
              .groups = 'drop'
    ) %>%
    mutate(progress = cats[1]) %>%
    plyr::rbind.fill(.data)
  
}

# count number of women who have not attended 
artifial_mulheres <- function(.data){
  
  .data%>%
    #fill mulheres faltantes
    group_by(target) %>%
    mutate(total = max(total, na.rm = T),
           interesadas = max(interesadas, na.rm = T),
           mulheres = total * prop_wb) %>%
    ungroup()
}

#=================================================================================


create_data_progress_FNM <- function(db_fnm, db_emprendedoras, by, cats){


  message(by)
  message(cats)
  
  if(by == "Seu todo"){
    
    #count total women in listas
    totais <- create_data_totais(filter(db_emprendedoras, grupo_accronym != "SGR"), by)
    
    #count women in different ranges of progress
    data_comp <- db_fnm %>%
      count_mulheres(actividade, progress) %>%
      mutate(target = "Seu todo") %>%
      left_join(totais, by = "target") %>%
      estimate_prop() %>%
    
    #create percentage of women whom have never attended %>% 
      artifitial_not_attended(cats = cats, actividade,target)
    
    
  } else if(by == "Por Cidade") {
    
    totais <- create_data_totais(filter(db_emprendedoras, grupo_accronym != "SGR"), by)
    
    data_comp <- db_fnm %>%
      count_mulheres(actividade, Cidade,progress) %>%
      rename(target = Cidade) %>%
      left_join(totais) %>%
      estimate_prop() %>%
      artifitial_not_attended(cats = cats,actividade,target) %>%
      artifial_mulheres()
    
  } else if(by == "Por Abordagem"){
    
    totais <- create_data_totais(filter(db_emprendedoras, grupo_accronym != "SGR"), by)
    
    data_comp <- db_fnm %>%
      count_mulheres(actividade, Abordagem,progress) %>%
      rename(target = Abordagem) %>%
      count_mulheres(actividade, target, progress) %>%
       left_join(totais, by = "target") %>%
      estimate_prop()  %>%
      artifitial_not_attended(cats = cats, actividade,target) %>%
      artifial_mulheres()
    
    
  }
  
  
  data_comp %>%
  mutate(progress = factor(progress,
                              levels = cats,
                           ordered = T
                           ),
            all = ifelse(progress %in% c(cats[4],cats[3]), prop_int, 0),
            actividade = forcats::fct_reorder(actividade, all))
  
  
}
