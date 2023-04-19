# library(rio)
# library(dplyr) 
# library(ggplot2)
# library(tidyr)
# 
# ####Grafico SGR
# #install_data_packages()
# 
# dir_master <- file.path(dirname(getwd()), "realiza_bancomundial")
# dir_data <- file.path(dir_master,"data")
# dir_lookups <- file.path(dir_data,"0look_ups") 
# emprendedoras <- import(file.path(dir_lookups,"emprendedoras.rds"))
# ## gravar as base de dados.
# all_presencas <- readRDS(paste(dir_master, "data/1.zoho/3.clean/all_presencas.rds", sep ="/"))
# fnm_presenca <- readRDS(paste(dir_master, "data/1.zoho/3.clean/fnm.rds", sep ="/"))
# sgr_presencas <- readRDS(paste(dir_master, "data/1.zoho/3.clean/sgr.rds", sep ="/"))
# emprendedoras_cresca=emprendedoras %>% dplyr::filter(grupo_accronym=="SGR")
# 
# #######participantes 
# 
# 
# 
# all_presencas11<-all_presencas
# table(all_presencas11$tipo_sessao_coaching)
# all_presencas11$tipo_sessao_coaching <- ""
# empreendedoras <- unique(all_presencas11$Emprendedora)
# 
# for (i in 1:length(empreendedoras)) {
#   emp <- empreendedoras[i]
#   count_data <- 1:nrow(all_presencas11)
#   count_data <- count_data[all_presencas11$Emprendedora == emp & all_presencas11$actividade == "Sessões de coaching"]
#   count_data_ord <- order(all_presencas11$data_posix[count_data])
#   count_data_ord <- count_data_ord[all_presencas11$actividade[count_data[count_data_ord]] == "Sessões de coaching"]
#   count_data <- count_data[count_data_ord]
#   for (j in 1:length(count_data)) {
#     all_presencas11$tipo_sessao_coaching[count_data[j]] <- paste0("Sessões de coaching", j)
#     all_presencas11$actividade[count_data[j]] <- paste0("Sessões de coaching", j)
#   }
# }
# 
# all_presencas11$actividade <- gsub("Sessões de coaching4", "Sessões de coaching3", all_presencas11$actividade)
# 
# all_presencas<-all_presencas11
# 
# 
# ###SGR
# filtro_cresca <- selectInput("by_cresca", 
#                           label = h4("Números da operação por:"),
#                           choices = c("Seu todo", "por cidade"))
# 
# ###SGR obrigatorias
# filtro_cresca_obr <- selectInput("by_cresca_obr", 
#                              label = h4("Números da operação por:"),
#                              choices = c("Seu todo", "por trimestre"))
# 
# ### tabela que calcula o numero de empreendedoras presentes por sessao por seu todo
# 
# 
# presencas_cresca_all<-all_presencas %>% dplyr::filter(presente==TRUE) %>%
#   group_by(Grupo, grupo_accronym,actividade, presente)  %>%  
#   summarise(n=n())
# 
# ### tabela que calcula o numero de empreendedoras presentes por sessao por cada cidade
# presencas_cresca_cidade<-all_presencas %>% dplyr::filter(presente==TRUE ) %>%
#   group_by(Cidade, Grupo, grupo_accronym,actividade, presente)  %>% 
#   summarise(n=n())  
# 
# 
# ###tabela que calcula numero de empreendedoras por cidade
#       ## sera utilizado para tracar as linhas pretas no grafico
# tab_cresca_cidade<-emprendedoras_cresca %>% filter(grupo_accronym %in% "SGR") %>% 
#   group_by(Cidade) %>% summarise(n=n())  
# 
# 
# 
# 
# 
# 
# 
# 
# ##grafico de cresca por todo
#   grafico_cresca_all<-presencas_cresca_all %>% dplyr::filter(grupo_accronym %in% "SGR") %>%
#   ggplot() +
#   #aes(text =paste("Presenciais:", n, "de", nrow(emprendedoras_cresca), "nas listas de BM"), x = actividade, y = n,fill = grupo_accronym) +
#   aes(text =paste("Presenciais:", n, "de", nrow(emprendedoras_cresca), "nas listas de BM"), x = actividade, y = n,fill = grupo_accronym) + 
#   geom_col() +
#   scale_fill_hue(direction = 1) +
#   theme_bw() +scale_fill_manual(name = "",
#                                 values = palette)+labs(
#                    y = "Numero de mulheres",
#                      x = ""
#                   )+theme(axis.text.x = element_text(angle = 90,
#                                                     size = 10),
#                         axis.text.y = element_text(size = 12)) + 
# geom_hline(aes(yintercept=nrow(emprendedoras_cresca)))+
#  scale_y_continuous(limits = c(0,400), breaks = seq(0,400,by=50))
# 
# 
# ##grafico de cresca por cidade
#   grafico_cresca_cidade<-presencas_cresca_cidade %>% filter(grupo_accronym %in% "SGR") %>%
#   ggplot() +
#   #aes(text =paste("Presenciais:", n, "de",tab_cresca_cidade$n, "nas listas de BM"), x = actividade, y = n, fill = Cidade) +
#   aes(text =paste("Presenciais:", n, "de",tab_cresca_cidade$n, "nas listas de BM"), x = actividade, y = n, fill = Cidade) +
#   geom_col() +
#   scale_fill_hue(direction = 1) +
#   theme_bw()+ facet_wrap(vars(Cidade))+
#   scale_fill_manual(name = "",
#                     values = palette)+
#   labs(
#     y = "Numero de mulheres",
#     x = ""
#   )+theme(axis.text.x = element_text(angle = 90,
#                                      size = 10),
#           axis.text.y = element_text(size = 12)) + 
#     geom_hline(data=tab_cresca_cidade,
#                aes(yintercept= n))+facet_wrap(vars(Cidade))+
#   scale_y_continuous(limits = c(0,170), breaks = seq(0,170,by=50))
# 
#  
# #######Sessoes obrigatorias  
#  
# presenca_obr_cresca_all=all_presencas %>% filter(grupo_accronym %in% "SGR", presente==TRUE)  %>% 
#   group_by(Cidade,Emprendedora) %>% summarise(n=n())  %>% filter(n>=9) %>% group_by(Cidade) %>% summarise(n=n())
#   
# ### Grafico sessao obrigatoria por todo 
# grafico_cresca_Obr_all<- presenca_obr_cresca_all %>%
#   ggplot() +
#   aes(text =paste("Presenciais:", n, "de",tab_cresca_cidade$n, "nas listas de BM"),
#       x = Cidade, y = n, fill=Cidade) +
#   geom_col() +
#   scale_fill_hue(direction = 1) +
#   theme_bw()+ 
#   scale_fill_manual(name = "",
#                     values = palette)+
#   labs(
#     y = "Numero de mulheres",
#     x = ""
#   )+theme(axis.text.x = element_text(angle = 0,
#                                      size = 10),
#           axis.text.y = element_text(size = 12)) + 
#   geom_point(data=tab_cresca_cidade,
#              aes(text =paste("Numero de empreendedoras:", n, "da lista do banco"),x=Cidade, y=n),shape = "triangle down filled", 
#              size =2, colour = "#112446")+
#   scale_y_continuous(limits = c(0,170), breaks = seq(0,170,by=20))
#  
# ###### Grafico sessao obrigatoria trimestre 
# 
# 
# 
# # TABELA REALIZA CRESÇA
# tbl_cresca_presente <- filter(all_presencas, Grupo == "Realiza & Cresça") %>%
#   group_by(Cidade, ID_BM,Emprendedora, presente) %>% 
#   summarise(total = n()) %>% 
#   filter(presente == TRUE)
# 
# tbl_cresca_ausente <- filter(all_presencas, Grupo == "Realiza & Cresça") %>%
#   group_by(Cidade, ID_BM,Emprendedora, presente) %>% 
#   summarise(total = n()) %>% 
#   filter(presente == FALSE)  
# 
# tbl_cresca <- merge(tbl_cresca_presente, tbl_cresca_ausente, by = "ID_BM") %>% 
#   select(ID_BM, Emprendedora.x, Cidade.x, total.x, total.y) %>%
#   rename(sessoes_presente = total.x, sessoes_ausente = total.y)
# 
# 
# tbl_cresca <- tbl_cresca %>% rename("Nr de sessões SGR que a empreendedora participou" = "sessoes_presente")
# tbl_cresca <- tbl_cresca %>% rename("Nome da empreendedora" = "Emprendedora.x")
# tbl_cresca <- tbl_cresca %>% rename("Cidade" = "Cidade.x")
# 
# tbl_cresca <- (select(tbl_cresca,-sessoes_ausente))
# 
# 
# # remoção de variaveis temporarias
# #rm(tbl_cresca_presente, tbl_cresca_ausente)
# 
# 
# pt <- list(
#   sProcessing = "Carregando...", sSearch = "Pesquisar&nbsp;:", 
#   sLengthMenu = "Exibir _MENU_ resultados por página", 
#   sInfo = "Mostrando de _START_ até _END_ de _TOTAL_ registros", 
#   sInfoEmpty = "Mostrando 0 até 0 de 0 registro(s)", 
#   sInfoFiltered = "(Filtrados de _MAX_ registros)", 
#   sInfoPostFix = "", sLoadingRecords = "Carregamento em curso...", 
#   sZeroRecords = "Nenhum registro encontrado", 
#   sEmptyTable = "Nenhum registro encontrado", 
#   oPaginate = list(
#     sFirst = "Primeiro", sPrevious = "Anterior", 
#     sNext = "Próximo", sLast = "Último"
#   ), 
#   oAria = list(
#     sSortAscending = ": Ordenar colunas de forma ascendente", 
#     sSortDescending = ": Ordenar colunas de forma descendente"
#   )
# )
