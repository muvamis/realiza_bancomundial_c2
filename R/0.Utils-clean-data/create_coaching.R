# count artificially sessoes de coaching
 #to count sessoes de coaching
create_coaching <- function(.data){
  
  .data %>%
    arrange(Emprendedora, data_posix, actividade) %>%
    group_by(Emprendedora, actividade) %>%
    mutate(actividade = ifelse(actividade == "Sessões de coaching",
                               paste(actividade, row_number()),
                               actividade)) %>%
    ungroup()
}


                           