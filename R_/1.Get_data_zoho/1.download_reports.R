#' Donwloads the reports from Zoho
#' Exports them into data/1.zoho
cli::cli_alert_info("Downloading reports")

#load dependencies
library(rio)
gmdacr::load_functions("functions")
exdir_zoho <- "data/1.zoho"
create_dir_(exdir_zoho)
exdir_raw <- file.path(exdir_zoho, "1.raw")
create_dir_(exdir_raw)


exfile <- file.path(exdir_raw, "reports_zoho.rds")


#Define reports to download ====================================================
# Only the dynamic reports from the system
# All the static ones are downloaded by R_/lookups/create_lookups.R

fetch_this <- c(
                FNM_grupal = "Presencas_grupales_FNM_Report", 
                FNM_ind = "Presencas_IND_FNM_Report",
                SGR_train = "Presencas_Fixas_roster_Report",
                SGR_ind = "Presencas_IND_SGR_report"
                # Calendar created by Tatiana with the details of all the events
                #"Calendario_Report")
)




# #download data ===============================================================
# it is saved in a list variable 


reportes <- purrr::map(fetch_this, download_realiza)
names(reportes) <- names(fetch_this)

#Export ========================================================================
# export list of reportes
export(reportes, exfile)

