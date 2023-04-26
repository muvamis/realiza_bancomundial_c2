
#vector of activities-----------------------------------------------------------
activities_fnm <- c(   "Sessão Inaugural" ,
                      "De mulher para mulher: Conecta!" ,
                      "Eventos de matchmaking" ,
                      "Eventos de networking" ,
                    #feira financeira
                      "Feira Financeira" ,
                      #"Modulos Obligatorios" ,
                    "Sessões de coaching",
                      "Sessões individuais",
                      "Workshops temáticos" 
                     )



#clean names for better display ---------------------------------------------
recode_fnm <- function(activities_fnm){
  
  recode(activities_fnm,  
         "Sessão Inaugural" = "Inaugural",
          "De mulher para mulher: Conecta!" = "Conecta",
          "Eventos de matchmaking" = "Matchmaking",
          "Eventos de networking" = "Networking",
         #feira financeira
          "Feira Financeira" = "Feiras",
          "Modulos Obligatorios" = "Modulos",
          "Sessões de coaching" = "Coaching",
          "Sessões individuais" = "Individuais",
          "Workshops temáticos" = "Workshops"
          )
  
}


# 
# #clean names for better display ------------------------------------------------
# clean_names_fnm <- function(.data){
#   
#   .data %>%
#     mutate(actividade_label = case_when(actividade == "De mulher para mulher: Conecta!" ~ "Conecta",
#                                         actividade == "Eventos de matchmaking" ~ "Matchmaking",
#                                         actividade == "Eventos de networking" ~ "Networking",
#                                         #feira financeira
#                                         actividade == "Feira Financeira" ~ "Feira",
#                                         actividade == "Modulos Obligatorios" ~ "Modulos",
#                                         actividade == "Sessões de coaching" ~ "Coaching",
#                                         actividade == "Sessões individuais" ~ "Individuais",
#                                         actividade == "Workshops temáticos" ~ "Workshops",
#                                         actividade == "Sessão Inaugural" ~ "Inaugural",
#                                         T ~ "Modulos Obligatorios"))
# }



