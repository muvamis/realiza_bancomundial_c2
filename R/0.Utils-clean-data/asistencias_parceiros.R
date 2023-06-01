#'@description It counts the times that a parceiro came to a session
#'@param path the path to the clean sgr.rds data
#'@returns a data frame. Each row is a emprendedora. And a column with the number
#'of times that a parceiro attended.

asistencias_parceiros <- function(.data, grupo){
  
  .data %>%
    dplyr::filter(grupo_accronym == grupo) %>%
    group_by(Emprendedora) %>%
    summarise(`Parceiro participou` = sum(Parceiro == "Presente", na.rm = T),
              .groups = 'drop')
}