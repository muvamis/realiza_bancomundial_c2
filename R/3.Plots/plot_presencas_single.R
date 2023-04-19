#Plot presensas single
#y  total mulheres
#x actividade

plot_presencas_single <- function(.data,
                                  x,
                                  y,
                                  hline 
                                  
){
  
  .data %>%
    ggplot(aes(x = {{x}},
               y = {{y}})) +
    geom_col(fill = palette[1]) +
    geom_hline(yintercept = hline) +
    scale_y_continuous(limits = c(0,hline +50),
                       breaks = seq(0, hline + 50, 50)) +
    labs(y = "Numero de mulheres",
         x = "") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme_realiza()
  
}