ui_participantes_SGR <- function(id){
  
  
  
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
        withSpinner(plotlyOutput(NS(id,"plot"),width="1000", height = "550px"), color = "black")
      ),
    )
  
    
    )
      
    
    
    
  )
}

#server 

server_participantes_SGR <- function(id, db_emprendedoras, db_presencas
                                    ){
  
  moduleServer(id, function(input, output, session){
    #identify_grupo() is defined in 1.Utils-app/identify-grupo.R
    #it detects the name of the grupo in the id of the module and returns a character
    #with the name as it is in the data (FNM or SGR or FNM + SGR)
    grupo_modulo <- identify_grupo(id)
    
    #Data for this module ----------------------------------------------------
    #presencas_de_grupo() is created un 0.utils-clean-data/presencas_de_grupo.R
    #it keeps the data for the given grupo, removes certain actividades that are 
    #not of interest for this analysis and keeps the given status.
    data_module <- presencas_de_grupo(presencas_db = db_presencas,
                                      grupo = grupo_modulo,
                                      #activities_fnm is created in 0.utils-clean-data/vector-actividades
                                      avoid_actividade = activities_fnm
    ) %>% 
      #artificially count sessoes de coaching
      # in the design of the Zoho form, it was not defined that sessoes de coaching
      #had to be sessoes de coaching 1, sessoes de coaching 2, etc.
      #This function creates that.
      # function creted in R/0.Utils-clean-data
      create_coaching() %>%
      # sort actividades as they ocurred
      mutate(
      actividade = factor(actividade,
                          #activities_sgr is created in 0.utils-clean-data/vector-actividades
                          levels = activities_sgr , #vector created in 1.Order-vectors
                          ordered = T
      ),
      actividade = recode_sgr(actividade) #In 0.utils-clean-data/vectors-actividade/vectors_SGR
      )
      
    
    
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
      
      #this function counts the number of emprendedoras registered in the programme
      #See details in 0.utils-cleandata/count_emprendedoras.R
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
      
      
      #add text to the charts--------------------------------------------------
      plot <- plot +
        geom_text(aes(label = total),
                  nudge_y = 5,
                  size = 4
                  )
      
      ggplotly(plot) %>%
        config(displayModeBar = F)
      # if(input$by_cresca == "Por cidade"){
      # 
      
      # 
      # }
      
      
    })
    
    
    })
  
}
