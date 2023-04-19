#'clean data for dashboard 
#' takes the data creted in 2.append_reports
#' Cleans and exports to dashboard
cli::cli_alert_info("Cleaning data")

library(rio)
library(dplyr)

#define input directories and files
indir <- "data/1.zoho/2.Append_raw"

infile_fnm <- file.path(indir,"fnm.rds")
infile_sgr <- file.path(indir,"sgr.rds")


#define exit directories  and files
exdir <- "data/1.zoho/3.clean"
create_dir_(exdir)
exfile_fnm <- file.path(exdir, "fnm.rds")
exfile_sgr <- file.path(exdir, "sgr.rds")
exfile_all <- file.path(exdir, "all_presencas.rds")

emprendedoras <- rio::import("data/0look_ups/emprendedoras.rds")


grupos <- c("fnm", "sgr")
infiles <- setNames(c(infile_fnm, infile_sgr), grupos)



#Clean data =====================================================================

clean_them <- lapply(grupos, function(x){
  #define file to read
  infile <- infiles[x]
  
  clean <- import(infile) %>%
    #clean dates (get rid of time, to be in portuguese, and create data_posix for vis)
    create_dates(Data) %>%
    #drop cases with empty date, or missing name of emprendedora
    drop_empty()  %>%
    #there are some emprendedoras that are in the system but that have not
    #been reported yet
    # thus, create a status of NA for them so we can flag it in the system
    #complete_emprendedoras(x,emprendedoras) %>%
   
    #Update status variable
    #Pendente : data evento <= today and status is missing
    #Agendado: data evento > today
    
    scheduled_status()  %>%
    
    #Create new columns to identify the status of the presneca:
      #presente, ausente, pendente, agendado
      # each of this takes a value of 1 or 0
    presente_ausente()
  
  }
  
  
  
)




names(clean_them) <- grupos



#export ========================================================================
export(clean_them$fnm, exfile_fnm)
export(clean_them$sgr, exfile_sgr)




#append both into a single file ================================================

all_presencas <- select(clean_them$fnm, 
                        Status,
                        Emprendedora,
                        Data,
                        actividade,
                        data_posix
                        ) %>%
  rbind(select(clean_them$sgr,
               Status,
               Emprendedora,
               Data,
               actividade = Modulo,
               data_posix,
               )) %>%
  left_join(emprendedoras, by = "Emprendedora") %>%
  #Only keep status that has been marked either as presente or ausente
  dplyr::filter(!Status %in% c("Agendado")) %>%
  mutate(presente = Status == "Presente",
         week = lubridate::week(data_posix)) 



export(all_presencas, exfile_all)

#remove temp objects
rm(indir, infile_fnm, infile_sgr, exdir, exfile_fnm, exfile_sgr, all_presencas)


# 
# View(clean_them$fnm)
# 
# fnm_clean <- import(infile_fnm) %>%
# #clean dates (get rid of time, to be in portuguese, and create data_posix for vis)
# create_dates(Data) %>%
#   #drop cases with empty date, missing status, or missing name of emprendedora
#   drop_empty() %>%
#   mutate(Status = scheduled_status(Status)) %>%
#   presente_ausente() 
# 
# 
# 
# 
# View(sgr_clean)
# #Clean SGR ====================================================================
# sgr_clean <- import(infile_sgr) %>%
#   #clean dates (get rid of time, to be in portuguese, and create data_posix for vis)
# create_dates(Data) %>%
#   #drop cases with emptu date, missing status, or missing name of emprendedora
#   drop_empty() %>%
#   mutate(Status = scheduled_status(Status)) %>%
#   presente_ausente() 
#                 
# export(sgr_clean, exfile_sgr)

