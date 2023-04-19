#plot participation by Cdade

plot_presencas_by <- function(.data,
                              num_emprendedoras,
                              x,
                              y,
                              fill){
  
  .data %>%
    ggplot(aes(x = {{x}},
               y = {{y}},
               fill = {{fill}})) +
    geom_col() +
    facet_wrap(vars({{fill}})) +
    geom_hline(data = num_emprendedoras,
               aes(yintercept = total)
    ) +
    scale_fill_manual(values = palette) +
    theme(axis.text.x =  element_text(angle = 90, hjust = 1)) +
    theme_realiza()
  
}


