# library(rio)
# library(dplyr) 
# library(ggplot2)
# library(tidyr)
# 
# ####Grafico SGR + FNM
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
# emprendedoras_conecta=emprendedoras %>% dplyr::filter(grupo_accronym=="FNM")
# 
# palette <- c("#A45EDA", "#F77333", "#5DD4C8", "#4497FD", "blue", 
#              "pink", "orange", "black", "green", 
#              "#D6B5ED", "FA8F5C")
# 
# all_presencas$abordagem <- ""
# 
# all_presencas$abordagem<-replace(all_presencas$abordagem, all_presencas$actividade==1.1, "SGR")
# all_presencas$abordagem<-replace(all_presencas$abordagem, all_presencas$actividade==1.2, "SGR")
# all_presencas$abordagem<-replace(all_presencas$abordagem, all_presencas$actividade==1.3, "SGR")
# all_presencas$abordagem<-replace(all_presencas$abordagem, all_presencas$actividade==2.1, "SGR")
# all_presencas$abordagem<-replace(all_presencas$abordagem, all_presencas$actividade==2.2, "SGR")
# all_presencas$abordagem<-replace(all_presencas$abordagem, all_presencas$actividade==2.3, "SGR")
# all_presencas$abordagem<-replace(all_presencas$abordagem, all_presencas$actividade==3.1, "SGR")
# all_presencas$abordagem<-replace(all_presencas$abordagem, all_presencas$actividade==3.2, "SGR")
# all_presencas$abordagem<-replace(all_presencas$abordagem, all_presencas$actividade==3.3, "SGR")
# all_presencas$abordagem<-replace(all_presencas$abordagem, all_presencas$actividade=="Sessao de encerramento de parceiros", "SGR")
# all_presencas$abordagem<-replace(all_presencas$abordagem, all_presencas$actividade=="Sessao intercalar de parceiros", "SGR")
# all_presencas$abordagem<-replace(all_presencas$abordagem, all_presencas$actividade=="Sessões de coaching", "SGR")
# all_presencas$abordagem<-replace(all_presencas$abordagem, all_presencas$actividade=="Introducao a sessao de parceiros", "SGR")
# all_presencas$abordagem<-replace(all_presencas$abordagem, all_presencas$abordagem=="", "FNM")
# 
# 
# #######participantes 
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
# 
# 
# 
# 
# data_conecta<-all_presencas %>% filter(Grupo=="Realiza & Conecta")
# 
# dados_agrupados <- data_conecta %>%
#   group_by(actividade) %>%
#   summarize(Agendadas = sum(n()),
#             presentes = sum(ifelse(Status== "Presente", 1, 0)))
# 
# dados_transformados <- gather(dados_agrupados, key = "Situacao", value = "n", `Agendadas`, `presentes`)
# #esquisser(dados_transformados)
# 
# 
# # Cria o gráfico de barras
# grafico_conecta_all <- ggplot(dados_transformados, aes(x = actividade)) +
#   geom_bar(aes(y = n, fill = Situacao), stat = "identity", position = position_dodge2(preserve = "single")) +
#   labs(title = "Agendadas VS presentes",
#        x = "ACtividade", y = "Total")+
#   theme_bw()+scale_fill_manual(name = "",
#                                values = palette)+labs(
#                                  y = "Total",
#                                  x = ""
#                                )+theme(axis.text.x = element_text(angle = 90,
#                                                                   size = 10),
#                                        axis.text.y = element_text(size = 12))
# 
# 
# 
# 
# 
# 
# #######Sessoes obrigatorias  
# 
# presenca_obr_conecta_all=all_presencas %>% filter(grupo_accronym %in% "FNM", presente==TRUE )  %>% 
#   group_by(Cidade,Emprendedora) %>% summarise(n=n())  %>% filter(n>=15) %>% group_by(Cidade) %>% summarise(n=n())
# 
# ### tabela que calcula o numero de empreendedoras presentes por sessao por cada cidade
# presencas_conecta_cidade<-all_presencas %>% dplyr::filter(presente==TRUE ) %>%
#   group_by(Cidade, Grupo, grupo_accronym,actividade, presente)  %>% 
#   summarise(n=n())  
# 
# 
# tab_conecta_cidade<-emprendedoras_conecta %>% filter(grupo_accronym %in% "FNM") %>% 
#   group_by(Cidade) %>% summarise(n=n())  
# 
# ### Grafico sessao obrigatoria por todo 
# grafico_conecta_Obr_all<- presenca_obr_conecta_all %>%
#   ggplot() +
#   aes(text =paste("Presenciais:", n, "de",tab_conecta_cidade$n, "nas listas de BM"),
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
#   geom_point(data=tab_conecta_cidade,
#              aes(text =paste("Numero de empreendedoras:", n, "da lista do banco"),x=Cidade, y=n),shape = "triangle down filled", 
#              size =2, colour = "#112446")+
#   scale_y_continuous(limits = c(0,200), breaks = seq(0,200,by=20))
# 
# ###### Grafico sessao obrigatoria trimestre 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # TABELA REALIZA CONECTA
# tbl_conecta_presente <- filter(all_presencas, Grupo == "Realiza & Conecta") %>%
#   group_by(Cidade, ID_BM,Emprendedora, presente) %>% 
#   summarise(total = n()) %>% 
#   filter(presente == TRUE)
# 
# tbl_conecta_ausente <- filter(all_presencas, Grupo == "Realiza & Conecta") %>%
#   group_by(Cidade, ID_BM,Emprendedora, presente) %>% 
#   summarise(total = n()) %>% 
#   filter(presente == FALSE)  
# 
# tbl_conecta <- merge(tbl_conecta_presente, tbl_conecta_ausente, by = "ID_BM") %>% 
#   select(ID_BM, Emprendedora.x, Cidade.x, total.x, total.y) %>%
#   rename(sessoes_presente = total.x, sessoes_ausente = total.y)
# 
# 
# tbl_conecta <- tbl_conecta %>% rename("Nr de sessões FNM que a empreendedora participou
# " = "sessoes_presente")
# tbl_conecta <- tbl_conecta %>% rename("Nome da empreendedora" = "Emprendedora.x")
# tbl_conecta <- tbl_conecta %>% rename("Cidade" = "Cidade.x")
# 
# tbl_conecta <- (select(tbl_conecta,-sessoes_ausente))
# 
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
