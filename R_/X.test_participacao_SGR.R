gmdacr::load_functions("R")

dir_master <- define_dir_master()
dir_data <- file.path(dir_master,"data")
dir_lookups <- file.path(dir_data,"0look_ups")

emprendedoras <- import(file.path(dir_lookups,"emprendedoras.rds"))

presencas <- create_data_presencas(dir_lookups, dir_data, c("Presente")) %>%
  dplyr::filter(actividade_label == "Modulos Obligatorios" | actividade == "Sessões de coaching") %>%
  filter(!actividade %in% c("Eventos de networking", "Feira Financeira")) %>%
  mutate(mulheres = 1)
names(presencas)

tabyl(presencas, actividade)

by = "Seu todo"


t <-create_data_participacao_SGR(presencas,emprendedoras, by=by) %>%
  mutate(Presencas = glue::glue("{mulheres}
                                 de {total} nas listas de BM"))
 
                   
                           
    

  View(t)
  