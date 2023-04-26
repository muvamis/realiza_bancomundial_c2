#'@return a list with parameters for the group:
#' abordagem 
#' avoid: activities to avoid in the data
#' obrigatorias: numero de sessoes obrigatorias
#' 

parameters_grupos <- function(grupo_modulo,
                              obrigatorias_sgr = 9,
                              obrigatorias_fnm = 15,
                              avoid = ""){
  
  if (grupo_modulo == "SGR"){
    
    abordagem <- "Cresça"
    #avoid sessao inaugural because it is not part of the mandatory SGR activities
    avoid = "Sessão Inaugural"
    #total de sessoes obrigatorias
    obrigatorias <-  obrigatorias_sgr
  } else if (grupo_modulo == "SGR + FNM"){
    
    abordagem <- "Movimenta"
    #sessoes obrigatorias fnm & sgr:
    obrigatorias <- obrigatorias_fnm + obrigatorias_sgr
    
  } else if (grupo_modulo == "FNM"){
    abordagem <- "Conecta"
    #sessoes obrigatorias fnm:
    # lkp_actividades <- import("data/0look_ups/actividades.rds")
    # obrigatorias_fnm <-sum(as.numeric(lkp_actividades$sessoes))
    
    
    obrigatorias <- obrigatorias_fnm
    
  }
  
  list(obrigatorias = obrigatorias,
       obrigatorias_fnm = obrigatorias_fnm,
       obrigatorias_sgr = obrigatorias_sgr,
       avoid = avoid,
       abordagem = abordagem
  )
  
}


