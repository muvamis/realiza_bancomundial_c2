#check that cumple with sessoes obrigatorias



tabela_sessoes_obrigatorias <- function(.data, 
                                        grupo_modulo,
                                        by = c("actividade_grupo","Cidade"),
                                        obrigatorias_sgr = 9,
                                        obrigatorias_fnm = 15){
  
  #count asistencias by the given groupo_by
  db_by <- .data %>%
    mutate(actividade_grupo = case_when(actividade %in% activities_sgr ~ "SGR",
                                        actividade %in% activities_fnm ~ "FNM")) %>%
    group_by_at(c("ID_BM",by)) %>%
    summarise(total_asistencias = n(),
              .groups = 'drop'
    ) %>%
    pivot_wider(id_cols = all_of(by),
                names_from = actividade_grupo,
                values_from = total_asistencias,
                values_fill = 0)

  #check that who has assisited to the sessoes obrigagorias
  if(grupo_modulo == "FNM"){


    db <-
      db_by %>%
      mutate(cumple = FNM >= obrigatorias_fnm)

  } else if( grupo_modulo == "SGR") {

    db <-
      db_by%>%
      mutate(cumple = SGR >= obrigatorias_sgr)

  } else if(grupo_modulo == "SGR + FNM" ) {

    db <- db_by %>%
      mutate(cumple_fnm = FNM >= obrigatorias_fnm,
             cumple_sgr = SGR >= obrigatorias_sgr,
             cumple = cumple_sgr & cumple_fnm)

  }

  return(db)

  
}
