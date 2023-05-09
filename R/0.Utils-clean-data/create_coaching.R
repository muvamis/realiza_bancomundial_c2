#' @description during the design and programming of the zoho tool, it was not
#' specified that the sessoes de coaching had to be identified by (1, 2, 3). thus,
#' this function counts the number of sessoes the coaching that a woman attended to.
#' And creates a variable name Sessoes de coaching 1, Sessoes de coaching 2, etc.
#' @param data Presencas

create_coaching <- function(.data){
  
  .data %>%
    arrange(Emprendedora, data_posix, actividade) %>%
    group_by(Emprendedora, actividade) %>%
    mutate(actividade = ifelse(actividade == "Sessões de coaching",
                               paste(actividade, row_number()),
                               actividade)) %>%
    ungroup()
}


                           