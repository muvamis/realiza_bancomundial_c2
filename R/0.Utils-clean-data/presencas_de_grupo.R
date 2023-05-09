#' @description There are 3 abordagems (FNM, SGR , FNM + SGR) and each has different
#' activities. This function keeps the records that the user needs in terms
#' of abordagem. 
#' @param presencas_db data frame of presencas
#' @param grupo group to keep the data from
#' @param avoid_actividade In case that a specific activity is to be avoid, this
#' can be one or a vector with many. Some times the user wants to take a certain activity away
#' @param keep The status to keep (Presente, Ausente)

#Keeps presencas of group of interest

presencas_de_grupo <- function(presencas_db,
                               grupo = c("SGR", "FNM"),
                               avoid_actividade = "Sessão Inaugural",
                               keep = "Presente"){
  
  
  data_return = presencas_db %>%
    #Keep only relevant records for this tab
    filter(grupo_accronym == grupo ,
           !actividade %in% avoid_actividade) %>%
    #only presentes
    filter(Status %in% keep)
   
  
  return(data_return)          
  
}

