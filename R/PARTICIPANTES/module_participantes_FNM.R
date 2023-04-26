ui_participantes_FNM <- function(id){
  
  
  
  tagList(
    sidebarLayout(
      
      sidebarPanel(
        width = 3,
        selectInput(NS(id,"periodo"), 
                    label = h4("Periodo"),
                    choices = choices_periodo)
        
        
      ),
      
      mainPanel(
        
        h4("O Banco seleccionou 350 mulheres para participar da abordagem conecta. 
         O gráfico abaixo mostra o numero de mulheres agendadas para as actividades 
         e numero das mulheres que participaram das actividades."),
        mainPanel(
          withSpinner(plotlyOutput(NS(id,"plot_emprendedoras"),width="1000", height = "550px"), color = "black")
        ),
      )
      
      
    )
    
    
    
    
  )
}

#server 

server_participantes_FNM <- function(id, db_emprendedoras, db_presencas
){
  
  moduleServer(id, function(input, output, session){
    
    grupo_modulo <- identify_grupo(id)
    
    #Data for this module ----------------------------------------------------
    data_module <- presencas_de_grupo(presencas_db = db_presencas,
                                      grupo = grupo_modulo,
                                      avoid_actividade = activities_sgr,
                                      keep = c("Presente", "Ausente", "Pendente")
                                      ) %>%
      mutate(actividade = factor(actividade,
                                 levels= activities_fnm,
                                 ordered = T),
             actividade =recode_fnm(actividade)
             )
    
    #reactive data emprendedoras -------------------------------------------------------------
    
    data_plot_emprendedoras <- reactive({
      data_module %>%
        group_by(actividade) %>%
        summarise(Agendadas = n(),
                  Presentes = sum(presente, na.rm = T)
                  ) %>%
        pivot_longer(-actividade,
                     names_to = "status",
                     values_to = "total")
      
    
    })
  
  #Create data that counts emprendedoras based on user selection
  # num_emprendedoras <- reactive({
  #   
  #   count_emprendedoras(emprendedoras_db = db_emprendedoras,
  #                                         grupo = grupo_modulo,
  #                                         agrupar_por = input$by_cresca)
  # })
  
  
  #plot ----------------------------------------------------------------------
  
  
  output$plot_emprendedoras <- renderPlotly({
    
    print(unique(data_plot_emprendedoras()$actividade))
    
    plot <- data_plot_emprendedoras() %>%
      ggplot(aes(x = actividade,
                 y = total,
                 fill = status)) +
      geom_col(width = .7,
               position = 'dodge2') +
      scale_fill_manual(values = palette) +
      labs(y = "Numero de Mulheres",
           x = ""
           ) +
      theme_realiza() +
      theme(axis.text.x = element_text(angle = 45))
    
    
    plot
    
    
    
  })
  
  
  })

}
