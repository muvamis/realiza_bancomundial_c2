#Creates data for random checks on data quality

create_random_check <- function(){
  
  
  #read all presencas (created in R_/1.Get_data_zoho/3.clean_reports.R)
  #and keep the latest record for an emprendedora marked as presente
  presencas <- rio::import("data/1.zoho/3.clean/all_presencas.rds") %>%
    dplyr::filter(Status == "Presente") %>%
    arrange(Emprendedora, Data) %>%
    group_by(Emprendedora) %>%
    dplyr::filter(row_number() == n()) %>%
    ungroup() 
  
  
  #select one Check per city
  selected <- sapply(unique(presencas$Cidade), function(cidade){
    
    db_cidade <- presencas %>% dplyr::filter(Cidade == cidade)
    
    sample(db_cidade$Emprendedora, 1)
    
    
  })
  
  
  #Keep only the selected ones.
  final <- presencas %>%
    dplyr::filter(Emprendedora %in% selected) %>%
    rename(Turma = Grupos_fixos)

 
  
}





