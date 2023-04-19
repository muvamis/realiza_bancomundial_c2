#data for dashboard
cli::cli_alert_info("Creating data for dashboard fnm")
Sys.setlocale("LC_ALL","Portuguese")
#'Imports clean data
#' takes the data creted in 2.append_reports
#' Cleans and exports to dashboard
#' Main actions: creates extra rows for activities that have not been scheduled yet
#' These activities are taken from the sessoes_obligatorias (look up table)
#' Output:
#' Data set with the statistics by emprendedora and actividade
#' Date set with divs for displayin in dash

library(rio)
library(dplyr)
library(tidyr)
#define input directories and files
indir <- "data/1.zoho/3.clean"
infile_fnm <- file.path(indir,"fnm.rds")
# infile_sgr <- file.path(indir,"sgr.rds")

#directory of lookuptables
dir_lkps <- "data/0look_ups"



#define exit directories  and files
exdir <- "data/2.Dashboard"
create_dir_(exdir)
exfile_fnm_stats <- file.path(exdir, "fnm_stats.rds")
exfile_fnm_div <- file.path(exdir, "fnm_div.rds")

#read data =====================================================================
actividades <- import(file.path(dir_lkps, "actividades.rds")) %>%
  select(-ID_actividade, - por) %>% distinct()

emprendedoras_lkp <- import(file.path(dir_lkps, "emprendedoras.rds"))
fnm_clean <- import(infile_fnm)


#Each activity has a number of mandatory sessions
#Thus, the dashboard wants to show how many sessions each woman has attended
#For this, the look up table of actividades is used 
# And a new row for each non reported activity is created

emprendedoras <- lapply(split(fnm_clean, fnm_clean$Emprendedora), function(emp){
  
  #not all the activities have been reported
  #Thus, join with lookup tables actividades to artificially create that
  emp_with_activities <- emp %>%
    full_join(select(actividades, actividade), by = "actividade") %>%
    mutate(Emprendedora = Emprendedora[1]
           #Agente = Agente[1]
           )
  

  activities <- lapply(split(emp_with_activities, emp_with_activities$actividade), function(act){
    
    emprendedora <- act$Emprendedora[1]
    activity <- act$actividade[1]
    
    #message(emprendedora)
   
    mandatory <- as.numeric(actividades$sessoes[actividades$actividade == activity])
    #print(activity)
    #message(emprendedora)
    #print(nrow(act))
    todo <- mandatory - nrow(act)
   

  #create only if todo is greater than 0
  if(todo > 0){
    
    #create artificial
    sessions_todo <- tibble(
      Emprendedora = rep(emprendedora, todo),
      actividade = rep(activity, todo)
      #Agente = rep(agente, todo)
    )
    
    
    return_this <- plyr::rbind.fill(act, sessions_todo) 
    
  } else {
    
    return_this <- act 
  }
   
    
    
    
  })
  
  #append activitites by emprendedora
  do.call(rbind, activities)
  
})


#append all emprendoras 
emprendedoras_dashboard <- do.call(rbind, emprendedoras) %>%
  
  mutate(#Pendente de agendar
         Status = ifelse(is.na(Status), "Pendente", Status),
         #mark ausente and pendente as 0 if the presenca has not been marked
         across(c(presente, ausente), function(x){ifelse(pendente == 1, 0, x)}),
         #Create div
         div = div_status(presente, ausente, agendado, pendente),) %>%
  #droping because it is a SGR activiry
  dplyr::filter(actividade != "Sessões de coaching") %>%
  arrange(Emprendedora,actividade, data_posix)




#Statistics by emprendedora ====================================================
#By emprendedora:
#displays the number of sessoes by actividade


fnm_stats <- emprendedoras_dashboard %>%
  #short the names of the actividades for frienlier display
  labels_actividades_fnm() %>%
  #cretes total sessoes, mean presensas, 
  count_asistencias() %>%
  #fetch cidade from Agente
  left_join(emprendedoras_lkp, by = "Emprendedora") %>%
  relocate(Cidade, grupo_accronym, Agente, Emprendedora)


#Data div =====================================================================
#this table is used to display the dots and colors by actividade

fnm_div <- fnm_stats %>%
  select(-ends_with("total"), -presente_promedio) %>%
  pivot_wider(id_cols = c(Cidade, grupo_accronym, Agente, Emprendedora),
              names_from = actividade_label,
              values_from = div)
            





#Export ========================================================================

export(fnm_stats, exfile_fnm_stats)
export(fnm_div, exfile_fnm_div)

last_refreshed <-Sys.time()
export(last_refreshed, file.path(exdir, "last_refreshed.rds"))

