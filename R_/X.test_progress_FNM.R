gmdacr::load_functions("R")

dir_master <- define_dir_master()
dir_data <- file.path(dir_master,"data")
dir_lookups <- file.path(dir_data,"0look_ups")

gray<-"#F5F5F5"
completed <- "#2A6B94"
moreHalf <- "#61C2B1"
lesHalf <- "#AAF2BB"


#Load actividades
actividades <- import(file.path(dir_lookups,"actividades.rds")) %>%
  select(actividade, sessoes) %>%
  distinct()


#emprendedoras 
emprendedoras <- import(file.path(dir_lookups,"emprendedoras.rds"))


#Load Presencas

presencas <- create_data_presencas(dir_lookups, dir_data, c("Presente") )


categories <- c("Has not participated","Less than half", "More than half", "Completed all sessoes")
#Track progress of emprendedoras agains targets
fnm <- presencas %>%
  #remove modulos (because it is SGR)
  filter(actividade != "Modulos Obligatorios",
         Abordagem != "SGR") %>%
  #Count presencas by actividade
  group_by(ID_BM,Cidade,Abordagem ,actividade) %>%
  summarise(presente = sum(presente),
            .groups = 'drop') %>%
  #join with actividades to get the number of mandatory sessions
  left_join(actividades) %>%
  #Estimate level of progress by emprendedora and actividade
  mutate(completas = presente/as.numeric(sessoes) * 100,
         #Create groups of progress
         progress = case_when(between(completas,0,49) ~ categories[2],
                              between(completas, 50,99) ~ categories[3],
                              completas > 99 ~ categories[4]
                              
         ) 
  ) %>%
  #Count by cidade
  group_by(Cidade, Abordagem,actividade, progress) %>%
  summarise(mulheres = n(),
            .groups = 'drop')

  


#count emprendedoras por
#by<- "Seu todo"
#by <- "Por Cidade"
by <- "Por Abordagem"




names(fnm)
by = "Seu todo"
cats = c("Ainda não participou","Participou em MENOS da metade das sessões",
         "Participou em MAIS de metade das sessões", "Completou todas as sessões")


data_plot <- create_data_progress_FNM(fnm, db_emprendedoras = emprendedoras,by = "Seu todo", cats = cats)
 



todo <- data_plot$target[1] == "Seu todo"

plot
#Plot--------------------------------------------------------------------------
plot <- data_plot %>%
  ggplot(aes
         (x = prop_int,
           y = actividade,
           fill = progress
         )
  ) +
  geom_col() +
  labs(y = "",
       x = "(%) Proportion of emprendedoras")+
  scale_fill_manual(values = rev(c(gray, lesHalf, moreHalf, completed)),
                    breaks = rev(categories)) +
  scale_x_continuous(labels = function(x){x*100}) +
  guides(fill = guide_legend(nrow = 2)) +
  theme_realiza()+
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.box.margin = margin(l = -3, unit = 'cm'),
        axis.text.y = element_text(hjust = 0)) 

#facet if neeeded
if(!todo){
  
  plot <- plot +
    facet_wrap(~target)
}
 


plot
# gmdacr::load_functions("R")
# View(fnm)
# 
# View(act)
# 
# presencas <- import(file.path(dir_data,"1.zoho/3.clean/all_presencas.rds"))

