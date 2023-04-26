asistencias_parceiros <- function(path = "data/1.zoho/3.clean/sgr.rds" ){
  
  import(path) %>%
    group_by(Emprendedora) %>%
    summarise(`Parceiro participou` = sum(Status_parceiro == "Presente", na.rm = T),
              .groups = 'drop')
}