library(rio)
library(janitor)
library(tidyr)
library(ggplot2)
library(shinycssloaders)


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
ui_evolucao_actividades <- function(id, periodo = "Semana"){
  
  
  tagList(
    
    sidebarLayout(
      
      sidebarPanel(width = 3,
                   selectInput(NS(id,"by"), 
                               label = h4("Números da operação por:"),
                               choices = selections_semana
                               
                   )
                   # checkboxGroupInput(NS(id,"checkGroup"), label = h4("Select an actividade"), 
                   #                    choices = list("Modulos" = 1, "Choice 2" = 2, "Choice 3" = 3),
                   #                    selected = 1)
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
serverEvolucaoActividades<- function(id, dir_data, db_emprendedoras, periodo = "Semana") {
  moduleServer(id, function(input, output, session) {
    
    
    dir_lookups <- file.path(dir_data,"0look_ups")
    
    
    #load presencas and define period (either month or week)-----------------------
    presencas <- reactive({
      
      #create data of presencas
      #imports clean/all_presencas.rds
      #keeps all status(Presente, Ausente y Pendente), creates month, names all modulos as modulos obligatorios
      presencas <- create_data_presencas(dir_lookups, dir_data, c("Presente", "Ausente", "Pendente"))
      
      #define periodo
      #renmaes the variable week or month as periodo so it can be aggregated later
      presencas <- define_var_periodo(presencas, periodo) %>%
        filter(actividade_label == "Modulos")
      
      presencas
      
    })
    
    
    #update checkbox ---------------------------------------------------------------
    
    labels <- reactive({
      
      unique(presencas()$actividade_label)
    })
    
    checkboxGroupInput
    observe({
      
      #vector with all activities
      labels <- unique(presencas()$actividade_label)
      
      #define values for choices
      choices <- lapply(seq(1:length(labels())), function(x){x})
      #assing labels to choices
      
      names(choices) <- labels()
      
      updateCheckboxGroupInput(session, "checkGroup", 
                               choices = choices,
                               selected = choices)
      
    })
    
    
    
    
    #get presencas over period -----------------------------------------------------
    
    
    # checked_indicators <- reactive({
    #   
    #   checked <- as.numeric(input$checkGroup)
    #   
    #   
    #   labels()[checked]
    #   
    #   
    # })
    # 
    # 

    data_evolucao <- reactive({

      create_data_evolucao_actividades(presencas(), input$by) %>%
        mutate(Presencias = glue("{presente}
                                  Esperadas: {esperadas}
                                 Taxa de participacao: {paste0(round(taxa*100,1),'%')}"
                                    )) 
      #%>%
       # filter(actividade_label %in% checked_indicators())

    })


    
    
    
    
    # Identify the number of variables in the data----------------------------------
    # if there are 3 it is because it was grouped by 2 variables
    
    cuantos_names <- reactive({
      
      length(names(data_evolucao()))
    })
    
    
    
    
    
    
    
    #Plot the data -----------------------------------------------------------------
    output$plot <- renderPlotly({
      
      
      base_plot <- data_evolucao() %>%
        ggplot(aes(x = periodo,
                   y = taxa,
                   #color = facet,
                   group = actividade_label,
                   label = Presencias
        )
        
        
        ) +
        geom_point() +
        
        geom_line(size = 1)
      
      
      
      #if it is a facet
      if(cuantos_names() ==7){
        
        plot <- base_plot +
          facet_wrap(~ facet)
        
        #elese
      } else {
        
        plot <- base_plot 
      }
      
      
      
      final_plot <- plot +
        expand_limits(y = 0) +
        #scale_fill_manual(values = palette)+
        labs(
          y = "Taxa de participacao (%)",
          x = periodo
        ) +
        theme_realiza() +
        scale_color_manual(values = c(palette),
                           name = "") +
        scale_y_continuous(labels = function(x){x*100},
                           limits = c(0,1,1.1)
                           )
      
      
      ggplotly(final_plot,
               tooltip = "label") %>%
        config(displayModeBar = F) 
      
    })
    
    
    
    
    output$header <- renderUI({
      
      HTML(
        glue("<h5>Os gráficos mostram a taxa de participação nas atividades <b>em relação às mulheres esperadas em cada atividade.</b> </h5>")
        
      )
    })
    
    
    observeEvent(input$by,{
      
      print(input$by)
      
    })
    
    
  })
  
  
}
