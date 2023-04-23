ui_participantes_SGR <- function(id, periodo = "Semana"){
  
  
  
  tagList(
  sidebarLayout(
    
    sidebarPanel(
      width = 3,
      selectInput(NS(id,"by_cresca"), 
                  label = h4("Números da operação por:"),
                  choices = c("Seu todo", "Por cidade"))
      
      
    ),
    
    mainPanel(
      
      h4("Número de mulheres que participaram de cada sessão. 
         A linha preta indica o número de mulheres incluídas nas listas do Banco Mundial."),
      mainPanel(
        withSpinner(plotlyOutput(NS(id,"plot"),width="1000", height = "650px"), color = "black")
      ),
    )
  
    
    )
      
    
    
    
  )
}

#server 

server_participantes_SGR <- function(id, db_emprendedoras, db_presencas,
                                     grupo_modulo = "SGR"){
  
  moduleServer(id, function(input, output, session){
    
    #Data for this module ----------------------------------------------------
    data_module <- presencas_de_grupo(presencas_db = db_presencas,
                                      grupo = grupo_modulo,
                                      avoid_actividade = activities_fnm
    ) %>% #function created in 0.Utils-clean-data
      
      #count_sessoes SGR
      arrange(Emprendedora, data_posix, actividade) %>%
      group_by(Emprendedora, actividade) %>%
      #artificially count sessoes de coaching
      mutate(actividade = ifelse(actividade == "Sessões de coaching",
                                 paste(actividade, row_number()),
                                 actividade
      ),
      actividade = factor(actividade,
                          levels = activities_sgr , #vector created in 1.Order-vectors
                          ordered = T
      )) %>%
      ungroup()
    
    
    #reactive data -------------------------------------------------------------
  
    data_plot <- reactive({
      
      #define group based on user options
      if(input$by_cresca == "Seu todo"){
        
        grupo = 'actividade'
        
      }
      
      if(input$by_cresca == "Por cidade"){
        
        grupo = c('actividade', 'Cidade')
      }
      
      
      
      data_plot <- data_module %>%
        group_by_at(grupo) %>%
        summarise(total = n(), .groups = 'drop')
      
      
      
    })
    
    #Create data that counts emprendedoras based on user selection
    num_emprendedoras <- reactive({
      
      count_emprendedoras(emprendedoras_db = db_emprendedoras,
                                            grupo = grupo_modulo,
                                            agrupar_por = input$by_cresca)
    })
    
    
    #plot ----------------------------------------------------------------------
    
    
    output$plot <- renderPlotly({
      
      print(names(data_plot()))
      if(input$by_cresca == "Seu todo"){
        plot <- plot_presencas_single(data_plot(),
                              x = actividade,
                              y = total,
                              hline = num_emprendedoras()$total[1]) 
        
      }
      
      if(input$by_cresca == "Por cidade"){
        
          plot <- plot_presencas_by(data_plot(),
                            num_emprendedoras = num_emprendedoras(),
                            x = actividade,
                            y = total,
                            fill = Cidade
                            )
       
      }
      
      plot
      # if(input$by_cresca == "Por cidade"){
      # 
      
      # 
      # }
      
      
    })
    
    
    })
  
}
