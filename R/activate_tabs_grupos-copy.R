#'Activate the server of the groups when their tab is selected
install_data_packages <- function() {
  packages <- c("ggthemes","RColorBrewer","esquisse", "sf","shiny", "shinythemes", "shinydashboard", "shinyWidgets", "DT", "ggplot2",  "plotly", "flexdashboard", "shinyjs","tidyverse", "data.table", "dplyr", "readr", "readxl", "googlesheets4", "jsonlite", "httr", "stringr", "lubridate", "janitor")
  # Verifica se cada pacote da lista está instalado e instala se não estiver
  # Verifica se cada pacote da lista está instalado e instala se não estiver
  for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package)
      
    } else {
    }
  }
  
  # Carrega todos os pacotes instalados
  #library(packages)
  # Carrega todos os pacotes instalados
  lapply(packages, library, character.only = TRUE)
  print("PACOTE INSTALADO E CARREGADO")
}


activate_tabs_grupos <- function(input, session, grupos, tipos){
  
  #walk each grupo
  lapply(grupos, function(grupo){
    
    #Create ids for tabs and servers (defined in the panels of each group)
    lapply(tipos, function(tipo){
      
      id_tab <- paste0(tipo,"_", grupo)
      id_server <- paste0(grupo,"_",tipo)
      
      #Activate the server of each tab when it is selected
      observe({
        
        if(input$Paneles == id_tab){
          
          #run server cidades
          if(tipo == "cidades"){
            
            serverCidade(id_server, grupo = grupo)
            
            #run server sessoes
          } else if(tipo == "agenda"){
            
            
            serverAgenda(id_server, grupo)
            
            
            }else {
            
            serverSessoes(id_server, grupo = grupo, tipo)
            
          }
          
        }
        #message(input$Paneles)
        
      })
      
      
      
    })
    
    
  })
  
}
