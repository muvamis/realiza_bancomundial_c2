#' Clean names of variables to make tables consistent across
#' Output: append FNM data and append SGR data
cli::cli_alert_info("Appending group and individual sessions")

library(rio)
library(dplyr)
gmdacr::load_functions("functions")

#this file is created in 1./get_data_zoho/1.download_reports.R
infile <- "data/1.zoho/1.raw/reports_zoho.rds"

#define exit directories and files
exdir <- "data/1.zoho/2.Append_raw"
create_dir_(exdir)
exfile_fnm <- file.path(exdir,"fnm.rds")
exfile_sgr <- file.path(exdir,"sgr.rds")





#read data ====================================================================
reportes_zoho <- import(infile)



#Append FNM ====================================================================
#Make report grupal and individual consistent 
# append them once consistent

#1. make names consistend of group table
fnm_group <- reportes_zoho$FNM_grupal %>%
  rename(Data = Nome_do_evento.Data_e_hora)




#2. The table individual does not have activitidade, so we create it artificially
fnm_ind <-reportes_zoho$FNM_ind %>% 
  distinct() %>%
  mutate(Actividades = "Sessões individuais") %>%
  rename(Data =  Data_do_registro)


#3. Append
fnm <- plyr::rbind.fill(fnm_group, fnm_ind) %>%
  distinct() %>%
  select(Status, 
         Nome_do_evento,
         Data,
         Emprendedora,
         Agente = Facilitadoras,
         actividade = Actividades) 


#4. Export

export(fnm, exfile_fnm)


# Append SGR ===================================================================

#1. train
sgr_train <- reportes_zoho$SGR_train %>%
  distinct() %>%
  select(Status = Presenca, 
         Status_parceiro = Parceiro,
         Data = Presencas_fixas.Date_field,
         Modulo = Presencas_fixas.Modulo,
         Facilitadora = Presencas_fixas.Facilitadora,
         Emprendedora = Emprendedoras
         )


#2. Individual
sgr_ind <- reportes_zoho$SGR_ind %>%
  distinct() %>%
  select(Status,
         Data= Data_do_registro,
         Emprendedora) %>%
  mutate(Modulo = "Sessões de coaching")


#3.Append
sgr <- plyr::rbind.fill(sgr_train, sgr_ind)



#4 . Export ===================================================================
export(sgr, exfile_sgr)






rm(infile, exdir , exfile_sgr, exfile_fnm, 
   fnm_group, fnm_ind,
   sgr_ind, sgr, sgr_train)

