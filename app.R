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
  
navbarMenu("C2",
           tabPanel(
             value = "cresca-participantes",
             title = "PARTICIPANTES",
             ui_participantes_SGR("cresca-participantes")
             
           )
           ),
navbarMenu("M2",
           tabPanel(
             value = "movimenta-participantes",
             title = "PARTICIPANTES SGR",
             ui_participantes_SGR("movimenta-participantes")
             
           )
),

##CRESÇA ======================================================================
  navbarMenu("CRESÇA",
    # tabPanel(value = "tab2", title = "PARTICIPANTES",
    #          # Define as colunas do layout de grade
    #          #ui_participacaoSGR("part_sgr")
    # 
    #          
    #          
    #          column(3,
    #                 #wellPanel(filtro_cresca),
    #                 
    #          ),
    #          
    #          
    #          column(9,h4("Número de mulheres que participaram de cada sessão. 
    #                      A linha preta indica o número de mulheres incluídas nas listas do Banco Mundial."),
    #                 mainPanel(
    #                   withSpinner(plotlyOutput("tab2_participante",width="1000", height = "650px"), color = "black")
    #                 ),
    #          ),             
    #          
    #          
    #          
    #                   
    #          ), 
    tabPanel(value = "tab3", title = "SESSÕES OBRIGATORIAS",
                      
             # Define as colunas do layout de grade
             
             
             #column(3,
             #       wellPanel(),
             #),
             
             
             column(12,h4("As empreendedoras da abordagem Cresça devem participar de pelo menos 9 sessões de SGR. O gráfico em baixo mostra o número de empreendedoras que cumprem o número de sessões obrigatórias. 
"),
                    mainPanel(
                      withSpinner(plotlyOutput("cresca_sessoes_obrigatorias",width="1000", height = "550px"), color = "black")
                    ),
             ),   
             
            
             ), 
    
    tabPanel(value = "tab4", title = "TABELA PARA SESSÕES OBRIGATORIAS",
             
             # Define as colunas do layout de grade
             column(12,h4("As empreendedoras da abordagem Cresça devem participar de pelo menos xx sessões de SGR. O gráfico em baixo mostra o número de empreendedoras que cumprem o número de sessões obrigatórias. 
"),
                    mainPanel(
                      withSpinner(dataTableOutput('tabelacresca'), color = "black")
                    ),
             ),             
             
    ),
    
  ), # CRESÇA 





##Movimenta ====================================================================
navbarMenu("MOVIMENTA",
           tabPanel(value = "tabmovimenta_SGR", title = "PARTICIPANTES SGR",
                    # Define as colunas do layout de grade
                    #ui_participacaoSGR("part_sgr")
                    
                    
                    
                    # Define as colunas do layout de grade
                    column(3,
                           #wellPanel(filtro_movimenta)
                           
                    ),
                    
                    
                    column(9,h4("Número de mulheres que participaram de cada sessão. 
                         A linha preta indica o número de mulheres incluídas nas listas do Banco Mundial."),
                           mainPanel(
                             withSpinner(plotlyOutput("movimenta_sgr",width="1000", height = "650px"), color = "black")
                           ),
                    ),             
                    
                    
           ), 
           tabPanel(value = "tabmovimenta_FNM", title = "PARTICIPANTES FNM",
                    
                   
                    column(6,
                           wellPanel(h4("colocamos o outro grafico")),
                    ),
                    
                    
                    column(6,h4("Espaco para texto explicativo"),
                           mainPanel(
                             withSpinner(plotlyOutput("movimenta_fnm",width="700", height = "650px"), color = "black")
                           ),
                    ),      
                    
                    
                    
                    
           ), 
           tabPanel(value = "sessoes_sgr_fnm", title = "SESSÕES OBRIGATORIAS",
                    column(3,
                           #wellPanel(filtro_movimenta_obr),
                    ),      
                    
                    column(9,uiOutput("texto_movimenta"),
                           mainPanel(
                             withSpinner(plotlyOutput("movimenta_sessoes_obrigatorias",width="1000", height = "550px"), color = "black")
                           ),
                    ), 
                    
           ), 
           
           tabPanel(value = "tabmovimenta_tabela", title = "TABELA PARA SESSÕES OBRIGATORIAS",
                    
                    # Define as colunas do layout de grade
                    column(12,h4(""),
                           mainPanel(
                             withSpinner(dataTableOutput('tabelamovimenta'), color = "black")
                           ),
                    ),             
                    
           ),
           
), # Movimenta 



##CONECTA =========================================================================
navbarMenu("CONECTA",
           tabPanel(value = "tabconecta_participantes", title = "PARTICIPANTES",
                    
                    column(6,
                           wellPanel(h4("colocamos o outro grafico")),
                    ),
                    
                    
                    column(6,h4("Espaco para texto explicativo"),
                           mainPanel(
                             withSpinner(plotlyOutput("conecta_participantes",width="700", height = "650px"), color = "black")
                           ),
                    ),        
                    
                    
           ), 
           
           tabPanel(value = "tabconecta_sessoes_obrigatorias", title = "SESSÕES OBRIGATORIAS",
                    
                    
                    column(12,h4("Espaco para texto explicativo"),
                           mainPanel(
                             withSpinner(plotlyOutput("conecta_sessoes_obrigatorias",width="1000", height = "550px"), color = "black")
                           ),
                    ),       
                    
           ), 
           
           tabPanel(value = "tabconecta_tabela", title = "TABELA PARA SESSÕES OBRIGATORIAS",
                    
                    # Define as colunas do layout de grade
                    column(12,h4(""),
                           mainPanel(
                             withSpinner(dataTableOutput('tabelaconecta'), color = "black")
                           ),
                    ),             
                    
           ),
), # CONECTA 


  )

)
  
   
# Define o servidor ============================================================
server <- function(input, output, session) {
 
  
  
  
  
  #define path to data (data is saved in repo)===========================
  
  dir_master <- file.path(dirname(getwd()), "realiza_bancomundial")
  dir_data <- file.path(dir_master,"data")
  dir_lookups <- file.path(dir_data,"0look_ups") 
  
  emprendedoras <- import(file.path(dir_lookups,"emprendedoras.rds"))
  
  ## gravar as base de dados.
  all_presencas <- readRDS(paste(dir_master, "data/1.zoho/3.clean/all_presencas.rds", sep ="/"))
  fnm_presenca <- readRDS(paste(dir_master, "data/1.zoho/3.clean/fnm.rds", sep ="/"))
  sgr_presencas <- readRDS(paste(dir_master, "data/1.zoho/3.clean/sgr.rds", sep ="/"))
  
  emprendedoras_cresca=emprendedoras %>% dplyr::filter(grupo_accronym=="SGR")
  #write_xlsx(all_presencas, paste(dir_master, "data/all_presencas.xlsx", sep ="/"))
  
  
  last_refreshed <- rio::import(file.path(dir_data,"2.Dashboard/last_refreshed.rds"))
  
  output$last_refreshed <- renderUI({
    
    text <- paste("Last data update:", last_refreshed)
    
    p(text)
  })
  
  
  

  
  observe({
  
  print(paste("Active tab: ", input$Paneles))
  activo <- input$Paneles
  
  if(activo == "overview-participantes") {
  
  serverParticipantes("Totals",  
                      emprendedoras_lp = emprendedoras, 
                      all_presencas = all_presencas )  
    
  } else if(activo == "cresca-participantes" ){
    
    #created in R/Participantes/module_participantes_SGR.R
    server_participantes_SGR(activo, 
                             db_emprendedoras = emprendedoras, 
                             db_presencas = all_presencas,
                             grupo_modulo = "SGR")
  } else if(activo == "movimenta-participantes" ){
    
    #created in R/Participantes/module_participantes_SGR.R
    server_participantes_SGR(activo, 
                             db_emprendedoras = emprendedoras, 
                             db_presencas = all_presencas,
                             grupo_modulo = "SGR + FNM")
  }
  
  })
  
  # } else if (activo == "tab 2"){
  #   
  # #serverParticipacaoSGR("part_sgr", dir_data, db_emprendedoras = emprendedoras, periodo = "Semana" )
  # 
  #   ### CRESCA PARTICIPANTE
  #   
  #   output$tab2_participante <- renderPlotly({
  #     
  #     
  #    if(input$by_cresca=="Seu todo") {
  #       
  #      grafico_cresca_all
  #      
  #       
  #     } else 
  #      if(input$by_cresca=="por cidade") {
  #       
  #       grafico_cresca_cidade
  #         
  #       } 
  #     
  #   })
  #    
  #   
  # } else if (activo == "tab3"){
  # 
  #   output$cresca_sessoes_obrigatorias <- renderPlotly({
  #     
  #     grafico_cresca_Obr_all
  #     
  #   })
  #   
  # 
  # } else if (activo == "tab4"){
  #   
  #  
  #   
  #    #output$tabelacresca <- renderDataTable(tabelacresca) 
  #     
  #     output$tabelacresca = DT::renderDT({
  #       tbl_cresca
  #     },
  #     extensions = 'Buttons',
  #     options = list(
  #       language = pt,
  #       dom = 'Blfrtip',
  #       buttons = list(
  #         list(
  #           extend = "excel",
  #           text = "Download"
  #         )
  #       )
  #     ))       
  #     
  #   
  # } else if (activo == "tabmovimenta_SGR"){
  #   
  #   
  #   ### MOVIMENTA PARTICIPANTE
  #   
  #   output$movimenta_sgr <- renderPlotly({
  #     
  #     if(input$by_movimenta=="Seu todo") {
  #       
  #       grafico_movimenta_all
  #       
  #     } else 
  #       if(input$by_movimenta=="por cidade") {
  #         
  #       grafico_movimenta_cidade
  #         
  #       } 
  #   })
  #   
  #   
  # } else if (activo == "tabmovimenta_tabela"){
  #   
  #   
  #   
  #   #output$tabelacresca <- renderDataTable(tabelacresca) 
  #   
  #   output$tabelamovimenta = DT::renderDT({
  #     tbl_movimenta
  #   },
  #   extensions = 'Buttons',
  #   options = list(
  #     language = pt,
  #     dom = 'Blfrtip',
  #     buttons = list(
  #       list(
  #         extend = "excel",
  #         text = "Download"
  #       )
  #     )
  #   ))     
  #   
  #   
  # } else if (activo == "tabconecta_tabela"){
  #   
  #   
  #   
  #   #output$tabelacresca <- renderDataTable(tabelacresca) 
  #   
  #   output$tabelaconecta = DT::renderDT({
  #     tbl_conecta
  #   },
  #   extensions = 'Buttons',
  #   options = list(
  #     language = pt,
  #     dom = 'Blfrtip',
  #     buttons = list(
  #       list(
  #         extend = "excel",
  #         text = "Download"
  #       )
  #     )
  #   ))      
  #   
  #   
  # } else if (activo == "tabmovimenta_FNM"){
  #   
  #   
  #   ### MOVIMENTA PARTICIPANTE
  #   
  #   output$movimenta_fnm <- renderPlotly({
  #     
  #     grafico_movimenta_participante_FNM
  #     
  #   })
  #   
  #   
  # }else if (activo == "sessoes_sgr_fnm"){
  #   
  #   
  #   ### MOVIMENTA obrigatorio
  #   
  #   output$movimenta_sessoes_obrigatorias <- renderPlotly({
  #     
  #     if(input$by_movimenta_obr=="Seu todo") {
  #       output$texto_movimenta <- renderText({
  #         HTML("<h4>As empreendedoras da abordagem Movimenta devem participar de pelo menos 9 sessões de SGR e 15 sessões de FNM. O gráfico em baixo mostra o número de empreendedoras que cumprem o número de sessões obrigatórias.</h4>")
  #       })
  #       grafico_movimenta_Obr_all
  #       
  #     } else {
  #       output$texto_movimenta <- renderText({
  #         HTML("<h4>O gráfico em baixo mostra o número de empreendedoras que cumprem apenas o número de sessões obrigatórias de SGR e FNM.</h4>")
  #       })
  #       grafico_movimenta_Obr_abord
  #         
  #     }
  #     
  #     
  #     
  #   })
  #   
  #   
  # }else if (activo == "tabconecta_participantes"){
  #   
  #   
  #   ### MOVIMENTA PARTICIPANTE
  #   
  #   output$conecta_participantes <- renderPlotly({
  #     
  #     grafico_conecta_all
  #     
  #   })
  #   
  #   
  # } else if (activo == "tabconecta_sessoes_obrigatorias"){
  #   
  #   
  #   ### MOVIMENTA PARTICIPANTE
  #   
  #   output$conecta_sessoes_obrigatorias <- renderPlotly({
  #     
  #     grafico_conecta_Obr_all
  #     
  #   })
  #   
  #   
  # }
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  
  #})

    
  
}
# Execute o aplicativo Shiny

shinyApp(ui, server)
