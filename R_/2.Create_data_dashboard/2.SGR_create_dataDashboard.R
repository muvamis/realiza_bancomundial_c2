#data for dashboard

#'Imports clean data
#' takes the data creted in 2.append_reports
#' Cleans and exports to dashboard
#' Main actions: creates extra rows for activities that have not been scheduled yet
#' These activities are taken from the sessoes_obligatorias (look up table)
#' Output:
#' Data set with the statistics by emprendedora and actividade
#' Date set with divs for displayin in dash
cli::cli_alert_info("Creating dashboard data for SGR")

library(rio)
library(dplyr)
library(tidyr)
#define input directories and files
indir <- "data/1.zoho/3.clean"
infile_sgr <- file.path(indir,"sgr.rds")

#directory of lookuptables
dir_lkps <- "data/0look_ups"



#define exit directories  and files
exdir <- "data/2.Dashboard"
create_dir_(exdir)
exfile_sgr_stats <- file.path(exdir, "sgr_stats.rds")
exfile_sgr_div <- file.path(exdir, "sgr_div.rds")

#read data =====================================================================
sessoes <- import(file.path(dir_lkps, "sessoes.rds"))
emprendedoras_lkp <- import(file.path(dir_lkps, "emprendedoras.rds"))

 


sgr_clean <- import(infile_sgr)



modulos <- c("Sessões de coaching", "1.1", "Introducao a sessao de parceiros", "1.2", "1.3",
             "2.1", "2.2", "2.3", "Sessao intercalar de parceiros",
             "3.1", "3.2", "3.3","Sessao de encerramento de parceiros")



emprendedoras <- lapply(split(sgr_clean, sgr_clean$Emprendedora), function(emp){
  
  emprendedora <- emp$Emprendedora[1]
  #grupo <- emp$Grupo[1]
  #cidade <- emp$Cidade[1]
  # get all sessoes fixas (so each emprendedora has all the sessoes)
  emp_with_sessoes <- emp %>%
    full_join(sessoes, by = "Modulo") %>%
    mutate(Emprendedora = Emprendedora[1],
           #Grupo = grupo,
           #Cidade = cidade
           )
  
  #Sessoes de coaching (3 mandatory)
  #Create rows of sessions that have not happened yet
  coachings <- sum(emp$Modulo ==  "Sessões de coaching", na.rm = T )
  todo <- 3 - coachings
  
  if(todo > 0){
    
    sessions_todo <- tibble(
      Emprendedora = rep(emprendedora, todo),
      Modulo = rep("Sessões de coaching", todo),
      #Grupo = rep(grupo, todo),
      #Cidade = rep(cidade, todo)
    )
    
    return_this <- return_this <- plyr::rbind.fill(emp_with_sessoes, sessions_todo)
    
  } else{
    
    return_this <- emp_with_sessoes
  }
  
  
})


#'Clean Modulo to be able to sort it ============================================
#'Cretate divs and divs of parceiro
emprendedoras_dashboard <- do.call(plyr::rbind.fill,emprendedoras)%>%
  mutate(Modulo = factor(Modulo,
                         levels = modulos,
                         ordered = T)) %>%
  arrange(Emprendedora,Modulo, data_posix) %>%
  mutate(
    #Identify type of activity
    actividade_label = case_when(grepl("coaching", Modulo) ~ "Coaching",
                          grepl("parceiro", Modulo) ~ "Modulos",
                          T ~ "Modulos"),
    div = div_status(presente, ausente, agendado, pendente),
    
    #Div Parceiro
    presente_parceiro = ifelse(Status_parceiro == "Presente", 1, 0),
    ausente_parceiro = ifelse(Status_parceiro == "Ausente", 1, 0),
    div_parceiro =  div_status(presente = presente_parceiro, ausente = ausente_parceiro, agendado, pendente)
         )%>%
  relocate(Emprendedora,actividade_label,Modulo, data_posix, Status, div) 


#artifically create rows of parceiros ==========================================

divs_parceiros <-  emprendedoras_dashboard %>%
  dplyr::filter(grepl("parceiro", Modulo)) %>%
  mutate(actividade_label = "Parceiros",
         div = div_parceiro,
         presente = presente_parceiro,
         ausente= ausente_parceiro) 


all_divs <- rbind(emprendedoras_dashboard, divs_parceiros)


#Statistics by emprendedora ====================================================
sgr_stats <-all_divs %>%
  count_asistencias() %>%
  left_join(emprendedoras_lkp, by = "Emprendedora") %>%
  rename(Turma = Grupos_fixos) %>%
  relocate(Cidade, grupo_accronym, Turma, Agente, Emprendedora) %>%
  #rename main variables to make it consistent with FNM
  #This helps the dashboard to have a single module
  select(-Agente,
         Agente = Turma
         )
  



  

#Data div =====================================================================
#this table is used to display the dots and colors by actividade

sgr_div <- sgr_stats %>%
  select(-ends_with("total"), -presente_promedio) %>%
  pivot_wider(id_cols = c(Cidade, Agente, Emprendedora, grupo_accronym),
              names_from = actividade_label,
              values_from = div)
  

          





#Export ========================================================================

export(sgr_stats, exfile_sgr_stats)
export(sgr_div, exfile_sgr_div)



