#plots sessoes obrigatorias

plot_obrigatorias <- function(.data,
                              num_emprendedoras){
  .data %>%
  ggplot(aes(y = total,
             x = Cidade,
             fill= Cidade)) +
    geom_col(width = .8) +
    labs(y = "Numero de mulheres",
         x = "") +
    geom_point(data = num_emprendedoras) +
    scale_fill_manual(values = palette) +
    theme_realiza()
}