

selections_semana <- setNames(
  #values
  c("Seu todo" ,
    "Por Cidade", 
    "Por Abordagem" 
    #"Por Cidade e Abordagem"
    #"Por Actividade no seu todo", 
    #"Por Actividade por Abordagem",  
    #"Por actividade por cidade" 
  ),
  #labels
  c("Seu todo" ,
    "Por Cidade", 
    "Por Abordagem" 
    #"Por Cidade e Abordagem"
    #"Por Actividade no seu todo", 
    #"Por Actividade por Abordagem",  
    #"Por actividade por cidade" 
  )
  
)

#UI ===========================================================================
#'@param periodo c("Semana", "Mes") it defines the labels of the selectors
ui_metas_sgr <- function(id){
  
  
  tagList(
    
    sidebarLayout(
      
      sidebarPanel(width = 3,
                   selectInput(NS(id,"by"), 
                               label = h4("Números da operação por:"),
                               choices = selections_semana
                               
                   )   
            
      ),
      mainPanel(
        uiOutput(NS(id,"header")),
        withSpinner(plotlyOutput(NS(id,"plot")), color = "black")
        
      )
    )
    
  )
}



#Server ======================================================================

#'@param periodo c("Semana", "Mes") defines whether to aggregate by semana or by mes
serverMetasSGR<- function(id, dir_data, db_emprendedoras) {
  moduleServer(id, function(input, output, session) {
    
 
    
    #read lookup de actividades ------------------------------------------------
    dir_lookups <- file.path(dir_data,"0look_ups")
    
    
    presencas <- reactive({
      
      create_data_presencas(dir_lookups, dir_data, c("Presente"))
    })
    
    
    #Count modulos completos in each city and compoenente -----------------------
    sgr <- reactive({
      
      presencas() %>%
        #remove modulos (because it is SGR)
        filter(actividade_label == "Modulos Obligatorios",
               Abordagem != "FNM") %>%
        #Count presencas by actividade
        group_by(ID_BM,Cidade,Abordagem) %>%
        summarise(presente = sum(presente),
                  .groups = 'drop') %>%
        mutate(completos = factor(as.character(presente),
                                  levels = as.character(seq(1,12)),
                                  ordered = T
        )
        ) %>%
        #Count by cidade
        group_by(Cidade, Abordagem, completos) %>%
        summarise(mulheres = n(),
                  .groups = 'drop')
      
      
    })
    
    
    #Create data plot
    data_plot <-reactive({
      
      create_data_progress_SGR(sgr(),db_emprendedoras, input$by) %>%
        mutate(across(c(prop_int, prop_wb), function(x){
          
          paste0(round(x * 100,1), "%")
          
        })) %>%
        mutate(Total = glue("de mulheres que han completado {completos} modulos: {mulheres}
                      {prop_wb} das listas de BM,
                      {prop_int} das interesadas
                      "))
      
      
    })
     
    
    
    
   
    

    
    
    #Plot the data -----------------------------------------------------------------
    output$plot<- renderPlotly({
      

      plot <- data_plot() %>%
        ggplot(aes
               (x = as.numeric(completos),
                 y = mulheres,
                 fill = target,
                 label = Total
               )
        ) +
        geom_col(position = "dodge",
                 width = .8) +
        geom_vline(xintercept = 10) +
        scale_x_continuous(breaks = seq(1,12,1),
                           labels = seq(1,12,1)) +
        scale_fill_manual(values = palette,
                          name = "") +
        labs(y = "Mulheres",
             x = "Sessões concluídas")+
        theme_realiza() +
        theme(legend.text = element_text(size = 8),
              axis.text.y = element_text(hjust = 0),
              axis.text = element_text(size = 10),
              axis.title = element_text(size = 12)) 
      
      
      
      ggplotly(plot,
               tooltip = "label")  %>%
        config(displayModeBar = F) %>%
        layout(
          legend = list(orientation = "h", x = 0.4, y = -0.2 )
          
               )
      
      
    })
    
    
    
    output$header <- renderUI({
      
      HTML(
        glue("<p>
            O SGR foi concebido para as mulheres participarem em 3 módulos de formação. 
            Cada módulo é dividido em 4 sessões, perfazendo um total de 12 sessões. 
            <b>O objetivo é que cada mulher complete pelo menos 10 sessões das 12 facilitadas</b>;</p>
            <p>O gráfico mostra o número de mulheres que completaram um certo 
            número de sessões. A linha preta indica o objetivo da participação.
            </p>")
        
      )
    })
    
    observeEvent(input$by,{
      
      print(input$by)
      
    })
    
    
  })
  
  
}
