
#vector of activities-----------------------------------------------------------
activities_fnm <- c(  "De mulher para mulher: Conecta!" ,
                      "Eventos de matchmaking" ,
                      "Eventos de networking" ,
                    #feira financeira
                      "Feira Financeira" ,
                      "Modulos Obligatorios" ,
                      #"Sessões de coaching",
                      "Sessões individuais",
                      "Workshops temáticos" ,
                      "Sessão Inaugural" )

#clean names for better display ------------------------------------------------
clean_names_fnm <- function(.data){
  
  .data %>%
    mutate(actividade_label = case_when(actividade == "De mulher para mulher: Conecta!" ~ "Conecta",
                                        actividade == "Eventos de matchmaking" ~ "Matchmaking",
                                        actividade == "Eventos de networking" ~ "Networking",
                                        #feira financeira
                                        actividade == "Feira Financeira" ~ "Feira",
                                        actividade == "Modulos Obligatorios" ~ "Modulos",
                                        actividade == "Sessões de coaching" ~ "Coaching",
                                        actividade == "Sessões individuais" ~ "Individuais",
                                        actividade == "Workshops temáticos" ~ "Workshops",
                                        actividade == "Sessão Inaugural" ~ "Inaugural",
                                        T ~ "Modulos Obligatorios"))
}



