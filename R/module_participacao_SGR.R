library(glue)
library(rio)
library(janitor)
library(tidyr)
library(ggplot2)
library(shiny)
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
ui_participacaoSGR <- function(id, periodo = "Semana"){
  
  
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
        withSpinner(
          plotlyOutput(NS(id,"plot"),
                       height = "600px")
          , color = "black")
        
      )
    )
    
  )
}



#Server ======================================================================

#'@param periodo c("Semana", "Mes") defines whether to aggregate by semana or by mes
serverParticipacaoSGR<- function(id, dir_data, db_emprendedoras, periodo = "Semana") {
  moduleServer(id, function(input, output, session) {
    
    
    dir_lookups <- file.path(dir_data,"0look_ups")
    
    
    #load presencas and define period (either month or week)-----------------------
    presencas <- reactive({
      
      #create data of presencas
      #imports clean/all_presencas.rds
      #keeps all status(Presente, Ausente y Pendente), creates month, names all modulos as modulos obligatorios
      
      presencas <- create_data_presencas(dir_lookups, dir_data, c("Presente")) %>%
        dplyr::filter(actividade_label == "Modulos Obligatorios" | actividade == "Sessões de coaching") %>%
        filter(!actividade %in% c("Eventos de networking", "Feira Financeira")) %>%
        mutate(mulheres = 1)
      
  
      presencas
      
    })
    
    
    #update checkbox ---------------------------------------------------------------
    
    labels <- reactive({
      
      unique(presencas()$actividade)
    })
    
   
    
    
    
 

    data_evolucao <- reactive({

      create_data_participacao_SGR(presencas(), db_emprendedoras,input$by) %>%
        mutate(Presencias = glue("{mulheres}
                                 de {total} nas listas de BM"
    
                                 
                                 )
                
               
               ) 
      #%>%
       # filter(actividade_label %in% checked_indicators())

    })


    
    
    
    
    # Identify the number of variables in the data----------------------------------
    # if there are 3 it is because it was grouped by 2 variables
    
    facet_it <- reactive({
      
     input$by != "Seu todo"
    })
    
    
    
    data_horizontal <- reactive({
      
      data_evolucao() %>%
        group_by(target) %>%
        summarise(total = max(total),
                  Presencias  = max(total)) %>%
        ungroup()
      
    })
    
    
    
    #Plot the data -----------------------------------------------------------------
    output$plot <- renderPlotly({
      
      print(names(data_evolucao()))
     
      
      base_plot <- data_evolucao() %>%
        ggplot(aes(x = actividade,
                   y = mulheres,
                   fill = target,
                   label = Presencias
        )
        
        
        ) +
        geom_col() +
        geom_hline(data = data_horizontal(),
                   aes(yintercept  = total,
                      
                       )
        ) 
      
      
      if(facet_it()){
        
        base_plot <- base_plot +
          facet_wrap(~ target)
      }
      
      final_plot <- base_plot +
          labs(
            y = "Numero de mulheres",
            x = ""
          ) +
        theme_realiza() +
        theme(axis.text.x = element_text(angle = 90,
                                         size = 10),
              axis.text.y = element_text(size = 12)) +
        scale_fill_manual(name = "",
                          values = palette)
        
      
      # 
      # #if it is a facet
      # if(cuantos_names() ==9){
      #   
      #   plot <- base_plot +
      #     facet_wrap(~ facet)
      #   
      #   #elese
      # } else {
      #   
      #   plot <- base_plot 
      # }
      # 
      # 
      # 
      # final_plot <- plot +
      #   expand_limits(y = 0) +
      #   #scale_fill_manual(values = palette)+
     
      #   scale_color_manual(values = c(palette),
      #                      name = "") +
      #   # scale_y_continuous(labels = function(x){x*100},
      #   #                    limits = c(0,1,1.1)
      #   #                    )
      # 
      
      ggplotly(final_plot,
               tooltip = "label") %>%
        config(displayModeBar = F) 
      
    })
    
    
    
    
    output$header <- renderUI({
      
      HTML(
        glue("<h5>Número de mulheres que participaram de cada sessão. A linha preta indica o número de mulheres incluídas nas listas do Banco Mundial. </h5>")
        
      )
    })
    
    
    observeEvent(input$by,{
      
      print(input$by)
      
    })
    
    
  })
  
  
}
