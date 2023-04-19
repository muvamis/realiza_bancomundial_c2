avg_presencas <- function(.data){
  
  .data %>%
    summarise(sessoes = sum(sessoes_total, na.rm = T),
              presencas = sum(presente_total, na.rm = T),
              presencas_avg = presencas/sessoes,
              .groups = 'drop'
    ) %>%
  mutate(presencas_avg_num = presencas_avg,
         presencas_avg = scales::percent(presencas_avg))
}

