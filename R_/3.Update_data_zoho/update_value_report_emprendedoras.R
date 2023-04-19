#Update information of emprendedoras in Zoho creator 

#Define parameters: 
var_to_update = "status_realiza" #use the name of the variable in Zoho
name_in_ref = "Confirmacao" #name of the variable in the reference data
query_update <- update_url_zoho("Emprendedoras_Report") #create the query url for this report



#read reference data ==========================================================
ref_data_path <- "data/0.reference/Resulados_Chamadas_Geral.xlsx"
sheets_ref <- openxlsx::getSheetNames(ref_data_path)


get_ref <- function(sheets){
  
  all_sheets <- lapply(sheets, function(x){
    
    rio::import(ref_data_path, sheet = x) %>%
      select(ID_BM = ID,
             #rename the variable so it is consistent with zoho
             {{var_to_update}}:= name_in_ref)
  })

  do.call(rbind, all_sheets)
    
}


ref_data <- get_ref(sheets_ref)


View(ref_data)
#update data in zoho =============================================================

#ref data must have 2 columns only (ID, and the variable to be updated)
# the variable to be updated must be named as it is in Zoho
#the query_update is the output of update_url_zoho()

update_now <- update_data_zoho(ref_data, query_update)







