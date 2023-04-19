lp_scripts <- list.files("R_/0.LookUps", full.names = T)
zoho_scripts <- list.files("R_/1.Get_data_zoho", full.names = T)
for_dashboard <- list.files("R_/2.Create_data_dashboard", full.names = T)
dir_lkps <- "data/0look_ups"



#Update look ups
lps <- lapply(lp_scripts, function(x){

  source(x, encoding = "UTF-8")

})


#Downlaod data from zoho
#Append groups and individual sessions
#Clean data
zoho_data <- lapply(zoho_scripts, function(x){


  source(x, encoding = "UTF-8")

})



dashboard_data <- lapply(for_dashboard, function(x){
  
  print(x)
  source(x, encoding = "UTF-8")
  
})


