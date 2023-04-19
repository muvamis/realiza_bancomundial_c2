#Module sessoes
library(dplyr)
library(plotly)
ui_agendas <- function(id, grupo){
  
  #Define the title of the main selector (agente for fnm, Turma for sgr)
  
  title_selector <- define_selector(grupo)
  
  tagList(
    
    
    sidebarLayout(
      sidebarPanel(width = 3,
                   #Inputs Cidade
                   selectInput(NS(id,"cidades"), "Cidade",
                               c("Beira", "Maputo", "Nampula")
                   ),
                   
                   selectInput(NS(id,"agente"), "Agente", choices = NA),
                   selectInput(NS(id,"mes"), "Mes", choices = NA),
                   selectInput(NS(id,"actividade"), "Actividade", choices = NA),
                   selectInput(NS(id,"evento"), "Evento", choices = NA)
                   
                   #Inputs Agente
                   # selectInput(NS(id,"agentes"), title_selector,
                   #             c("")
                   
      ),
      mainPanel(width = 9,
                #Header of Agente (name and % of assistance)
                uiOutput(NS(id,"header")),
                fluidRow(
                  
                  column(8,
                         plotlyOutput(NS(id,"plot"))
                  ),
                  
                  column(4,
                         #table with presences
                         withSpinner(DT::DTOutput(NS(id,"table")), color = "black")
                  )
                )
                
                
      )
    )
    
    
    
  )
  
}



#Server ======================================================================
serverAgenda <- function(id, grupo) {
  moduleServer(id, function(input, output, session) {
    
    message(grupo)
    if(grupo == "fnm"){
      
      sel_grupo = "FNM"
    } else {
      
      sel_grupo = "SGR + FNM"
    }
    
    #read data --------------------------------------------------------------
    all <- rio::import("data/1.zoho/3.clean/all_presencas.rds")
    db_componente <- all %>% 
      dplyr::filter(actividade != "Sessões individuais") %>%
      dplyr::filter(grupo_accronym == sel_grupo)
    
    
    
    
    
    
    #Reactive elements ========================================================
    
    #react to selected cidade
    data_cidade <- reactive({
      
      db_componente %>%
        dplyr::filter(Cidade == input$cidades) 
    })
    
    agentes_reactive <- reactive({
      
      sort(unique(data_cidade()$Agente))
      
    })
    
    #agentes -------------------------------------------------------------------
    #update options of input agentes
    observeEvent(agentes_reactive(),{
      
      updateSelectInput(session, "agente",
                        choices = agentes_reactive(),
                        
      )
      
    })
    
    
    data_agente <- reactive({
      
      data_cidade() %>% dplyr::filter(Agente == input$agente) %>% 
        arrange(desc(data_posix))
    })
    
    
    #meses --------------------------------------------------------------------
    meses_reactive <- reactive({
      
      unique(data_agente()$month)
      
    })
    
    observeEvent(meses_reactive(),{
      
      updateSelectInput(session, "mes",
                        choices = meses_reactive())
    })
    
    
    data_mes <- reactive({
      data_agente() %>% dplyr::filter(month == input$mes)
    })
    
    #actividade ----------------------------------------------------------------
    
    
    actividade_reactive <- reactive({
      
      unique(data_mes()$actividade)
    })      
    
    observeEvent(actividade_reactive(), {
      
      updateSelectInput(session, "actividade",
                        choices = actividade_reactive())
      
    })
    
    data_actividade <- reactive({
      
      data_mes() %>% dplyr::filter(actividade == input$actividade)
      
    })
    
    #evento ------------------------------------------------------------------
    evento_reactive <- reactive({
      
      sort(unique(data_actividade()$Nome_do_evento))
      
    })
    
    
    observeEvent(evento_reactive(), {
      
      updateSelectInput(session, "evento",
                        choices = evento_reactive())
    })
    
    
    data_evento <- reactive({
      
      data_actividade() %>% dplyr::filter(Nome_do_evento == input$evento)
    })
    
    
    
    #Header ===================================================================
    
    output$header <- renderUI({
      
      
      tags$div(class = "text-center",
        h1(input$agente),
        #h4(paste("Tipo de actividade:", input$actividade)),
        h5(paste("Evento:",input$evento, "-", unique(data_evento()$Data)))
      )
      
    })
    
    output$plot <- renderPlotly({
      
      
      db_plot <- data_evento() %>%
        group_by(Agente) %>%
        summarise(Agendadas = n(),
                  Presentes = sum(Status == "Presente")) %>%
        pivot_longer(-Agente,
                     names_to = "indicador",
                     values_to = "Emprendedoras")
      
      
      plot <- ggplot(db_plot,
                     aes(x = indicador,
                         y = Emprendedoras,
                         fill = indicador)
      ) +
        geom_col() +
        labs(x = "",
             y = "Emprendedoras") +
        scale_y_continuous(labels = function(x)round(x,0)) +
        scale_fill_manual(values = palette) +
        theme_realiza()
      
      
      
      ggplotly(plot,
               tooltip = 'y') %>%
        config(displayModeBar = F) %>%
        layout(
          legend = list(orientation = "h", x = 0.4, y = -0.2 )
          
        )
      
      
    })
    
    
    
   
    output$table <- DT::renderDT({
      
      
      DT::datatable(
        data_evento() %>%
          select(Emprendedora, Status),
        escape = F,
        rownames = F,
        options = list(pageLength = nrow(data_evento()),
                       dom = 't',
                       ordering = F,
                       selector = "td:not(.not-selectable)")
      )
      
    })
    
  })
}