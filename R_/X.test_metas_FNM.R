gmdacr::load_functions("R")

dir_master <- define_dir_master()
dir_data <- file.path(dir_master,"data")
dir_lookups <- file.path(dir_data,"0look_ups")

gray<-"#F5F5F5"
completed <- "#2A6B94"
moreHalf <- "#61C2B1"
lesHalf <- "#AAF2BB"


categories <- 
  c("Ainda não participou","Participou em MENOS da metade das sessões",
    "Participou em MAIS de metade das sessões", "Completou todas as sessões")

#Load actividades
actividades <- import(file.path(dir_lookups,"actividades.rds")) %>%
  select(actividade, sessoes) %>%
  distinct()

#emprendedoras 
emprendedoras <- import(file.path(dir_lookups,"emprendedoras.rds"))


#Load Presencas

presencas <- create_data_presencas(dir_lookups, dir_data, c("Presente") )
fnm <- presencas %>%
  #remove modulos (because it is SGR)
  filter(actividade_label != "Modulos Obligatorios",
         Abordagem != "SGR") 
  #Count presencas by actividade
  group_by(ID_BM,Cidade,Abordagem ,actividade) %>%
  summarise(presente = sum(presente),
            .groups = 'drop') %>%
  #join with actividades to get the number of mandatory sessions
  left_join(actividades) %>%
  #Estimate level of progress by emprendedora and actividade
  mutate(completas = presente/as.numeric(sessoes) * 100)
         #Create groups of progress
         progress = case_when(between(completas,0,49) ~ categories()[2],
                              between(completas, 50,99) ~ categories()[3],
                              completas > 99 ~ categories()[4]
                              
         ) 
  ) %>%
  #Count by cidade
  group_by(Cidade, Abordagem,actividade, progress) %>%
  summarise(mulheres = n(),
            .groups = 'drop')

View(fnm)
