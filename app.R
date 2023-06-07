library(shiny)
library(rio)
#library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(stringr)
#library(sf)
#library(RColorBrewer)
library(shinycssloaders)
library(shinythemes)
#library(readxl)
library(DT)
#library(shinyWidgets) 
#library(writexl)

library(ggthemes)
#library(haven)







# Define UI
ui <- fluidPage(
  
  # Define o tema
  theme = shinytheme("flatly"),
  
  uiOutput("last_refreshed"),
  
  # Define a barra de navegação
  navbarPage(
    
    title = "REALIZA" ,  
    id = "Paneles",
    tags$head(tags$style(HTML('.navbar-default {background-color: #76004B;}'))),
    
    
    #Overview ==================================================================
    navbarMenu("OVERVIEW", 
               icon=icon("exchange-alt"),
               tabPanel(
                 title = "PARTICIPANTES",
                 value = "overview-participantes",
                 icon=icon("chart-line"),
                 # Define as colunas do layout de grade
                 ui_participantes("Totals")
                 
               ), 
               
               
    ), # OVERVIEM
    
    #CRESCA -----------------------------------------------------------------------
    navbarMenu("Cresça",
               tabPanel(
                 value = "cresca-participantes",
                 title = "PARTICIPANTES",
                 ui_participantes_SGR("cresca-participantes")
               ),
               tabPanel(
                 value = "cresca-chart-obrigatorias",
                 title = "SESSÕES OBRIGATORIAS",
                 ui_sessoes_obrigatorias("cresca-chart-obrigatorias")
               ),
               tabPanel(
                 value = "cresca-tabela-obrigatorias",
                 title = "TABELA PARA SESSÕES OBRIGATORIAS",
                 ui_sessoes_obrigatorias("cresca-tabela-obrigatorias")
               )
    ),
    
    #Movimenta --------------------------------------------------------------------
    navbarMenu("Movimenta",
               tabPanel(
                 value = "movimenta-participantes",
                 title = "PARTICIPANTES SGR",
                 ui_participantes_SGR("movimenta-participantes")
                 
               ),
               
               tabPanel(
                 value = 'movimenta-participantes-fnm',
                 title= "PARTICIPANTES FNM",
                 ui_participantes_FNM('movimenta-participantes-fnm')
               ),
               
               tabPanel(
                 value = "movimenta-chart-obrigatorias",
                 title = "SESSÕES OBRIGATORIAS",
                 ui_sessoes_obrigatorias("movimenta-chart-obrigatorias")
               ),
               
               tabPanel(
                 value = "movimenta-tabela-obrigatorias",
                 title = "TABELA PARA SESSÕES OBRIGATORIAS",
                 ui_sessoes_obrigatorias("movimenta-tabela-obrigatorias")
               )
    ),
    
    #Conecta -----------------------------------------------------------------------
    
    navbarMenu("Conecta",
               
               tabPanel(
                 
                 value = 'conecta-participantes-fnm',
                 title= "PARTICIPANTES",
                 ui_participantes_FNM('conecta-participantes-fnm')
               ),
               
               tabPanel(
                 value = "conecta-chart-obrigatorias",
                 title = "SESSÕES OBRIGATORIAS",
                 ui_sessoes_obrigatorias("conecta-chart-obrigatorias")
               ),
               
               tabPanel(
                 value = "conecta-tabela-obrigatorias",
                 title = "TABELA PARA SESSÕES OBRIGATORIAS",
                 ui_sessoes_obrigatorias("conecta-tabela-obrigatorias")
               )
    ),
    
    
   



  )

)


# Define o servidor ============================================================
server <- function(input, output, session) {
  
  today <- Sys.Date()
  abordagems <- c("cresca", "movimenta", "conecta")  
  #define path to data (data is saved in repo)
  #data is saved in the realiza repo everytime that the admin updates it
  dir_data <- define_dir_data()
  dir_lookups <- file.path(dir_data,"0look_ups") 
  
  
  #read data --------------------------------------------------------------------
  emprendedoras <- rio::import(file.path(dir_lookups,"emprendedoras.rds"))
  
  
  all_presencas <- rio::import(file.path(dir_data, "2.clean_presencas.rds")) %>%
    filter(Nome_do_evento != "") %>%
    mutate(data_posix = lubridate::dmy(str_sub(data_evento, 1,11))) %>%
   #create month
    mutate(date = dmy(str_sub(data_evento, 1,12)),
           mes = lubridate::month(date),
           #adjust mes
           mes = mes - min(mes, na.rm = T) + 1
           
    ) %>%
    #keep only events that have happened already
    filter(date <= today)
           
    
  
 
  
  
  emprendedoras_cresca=emprendedoras %>% dplyr::filter(grupo_accronym=="SGR")
  
  
  
  last_refreshed <- rio::import(file.path(dir_data,"last_refreshed.rds"))
  
  output$last_refreshed <- renderUI({
    
    text <- paste("Last data update:", last_refreshed)
    
    p(text)
  })
  
  #Define tabs ----------------------------------------------------------------
  #I am doing this to avoid copy and pasting servers and parameters
  tabs_participantes_SGR <- paste0(abordagems, "-participantes")
  
  tabs_obrigatorias <- c(
    paste0(abordagems,"-tabela-obrigatorias"),
    paste0(abordagems,"-chart-obrigatorias"))
 
  tabs_participantes_FNM <- paste0(abordagems, '-participantes-fnm')
  

  #Activate servers
  
  observe({
    activo <- input$Paneles
    print(paste("Active tab: ", input$Paneles))
    
    #Server overview ===========================================================
    if(activo == "overview-participantes") {
      
      serverParticipantes("Totals",
                          emprendedoras_lp = emprendedoras,
                          all_presencas = all_presencas )
    }
    
    
    #server participantes SGR =======================================================
    lapply(tabs_participantes_SGR, function(tab){
      
      activo <- input$Paneles
      if(activo == tab){
        #created in R/Participantes/module_participantes_SGR.R
        server_participantes_SGR(activo,
                                 db_emprendedoras = emprendedoras,
                                 db_presencas = all_presencas
                                 #grupo_modulo = identify_grupo(tab)
                                 )
      }
      
      
    })
    
    
    #server sessoes obrigatorias ===============================================
    lapply(tabs_obrigatorias, function(tab){
      
      if(activo == tab){
        
        server_sessoes_obrigatorias(activo,
                                    db_emprendedoras = emprendedoras, 
                                    db_presencas = all_presencas,
                                    #grupo_modulo = identify_grupo(tab)
                                    #identigy_mode() is created in 1.utils-app/identify_mode.R
                                    #it identifies whether the id contains tabela or chart
                                    mode = identify_mode(tab)
                                    )
      }
      
    })
    
    #server participantes FNM
    lapply(tabs_participantes_FNM, function(tab){
      
      activo <- input$Paneles
      if(activo == tab){
        #created in R/Participantes/module_participantes_SGR.R
        server_participantes_FNM(activo,
                                 db_emprendedoras = emprendedoras,
                                 db_presencas = all_presencas
                                 #grupo_modulo = identify_grupo(tab)
        )
      }
      
      
    })
  
    
    
  })
  
 
  
    
  
  
}
# Execute o aplicativo Shiny

shinyApp(ui, server)
