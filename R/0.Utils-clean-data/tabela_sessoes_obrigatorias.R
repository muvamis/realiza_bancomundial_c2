#check that cumple with sessoes obrigatorias
#' @returns A table used in TABELAS OBRIGATORIAS that includes the following columns:
#' ID_BM, EMprendedora, Cidade, Numero Sessoes FNM, Numero Sessoes SGR, Sessoes Parceiro
#' @param data clean data of presencas (ALL presencas)
#' @param by Variabels para agrupar a tabela
#' @param obrigatorias_sgr numero de sessoes obrigatorias_sgr
#' @param obrigatorias_fnm numero de sessoes obrigatorias_fnm
#' @param column_SGR nombre para a columna de sessoes SGR
#' @param column_FNM nombre para a columna de sessoes FNM



tabela_sessoes_obrigatorias <- function(.data, 
                                        grupo_modulo,
                                        by = c("actividade_grupo","Cidade"),
                                        obrigatorias_sgr = 9,
                                        obrigatorias_fnm = 15,
                                        column_SGR = "Sessões SGR",
                                        column_FNM = "Sessões FNM"
                                        ){
  

  db_by <- .data %>%
    #identify whether the activity belongs to FNM or SGR
    mutate(actividade_grupo = case_when(actividade %in% activities_sgr ~ column_SGR,
                                        actividade %in% activities_fnm ~ column_FNM)) %>%
    #Count asistencias by emprendedora and the variables given in by
    group_by_at(c("ID_BM",by)) %>%
    summarise(total_asistencias = sum(Status == "Presente"),
              total_agendadas = n(),
              .groups = 'drop'
    ) %>%
    #re-formatear tabela para poder ter uma columna independente do numero de actividades de SGR and FNM
    pivot_wider(id_cols = all_of(by),
                names_from = actividade_grupo,
                #values_from = c(total_asistencias, total_agendadas),
                values_from = total_asistencias, 
                values_fill = 0)

  #check that who has assisited to the sessoes obrigagorias
  if(grupo_modulo == "FNM"){


    db <-
      db_by %>%
      mutate(cumple = .[column_FNM] >= obrigatorias_fnm)

  } else if( grupo_modulo == "SGR") {

    db <-
      db_by%>%
      mutate(cumple = .[column_SGR] >= obrigatorias_sgr)

  } else if(grupo_modulo == "SGR + FNM" ) {

    db <- db_by %>%
      mutate(cumple_fnm = .[column_FNM] >= obrigatorias_fnm,
             cumple_sgr = .[column_SGR] >= obrigatorias_sgr,
             cumple = cumple_sgr & cumple_fnm)

  }

  
  return(db)

  
}
