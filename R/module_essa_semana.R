library(rio)
library(janitor)
library(tidyr)
library(ggplot2)
library(shinycssloaders)


selections_semana <- setNames(
  #values
  c("Seu todo" ,
    "Por Cidade", 
    "Por Abordagem", 
    "Por Cidade e Abordagem",
    "Por Actividade no seu todo", 
    "Por Actividade por Abordagem",  
    "Por actividade por cidade" 
  ),
  #labels
  c("Seu todo" ,
    "Por Cidade", 
    "Por Abordagem", 
    "Por Cidade e Abordagem",
    "Por Actividade no seu todo", 
    "Por Actividade por Abordagem",  
    "Por actividade por cidade" 
  )
  
)

#UI ===========================================================================
#'@param periodo c("Semana", "Mes") it defines the labels of the selectors
ui_essa_semana <- function(id, periodo = "Semana"){
  
  
  tagList(
    
    sidebarLayout(
      
      sidebarPanel(width = 3,
                   selectInput(NS(id, "periodo"),
                               label = h4(glue("{periodo} da operação")),
                               choices = ""),
                   selectInput(NS(id,"by"), 
                               label = h4("Números da operação por:"),
                               choices = selections_semana
                               
                   )
      ),
      mainPanel(
        uiOutput(NS(id,"header")),
        withSpinner(plotOutput(NS(id,"plot")), color = "black")
        
      )
    )
    
  )
}



#Server ======================================================================

#'@param periodo c("Semana", "Mes") defines whether to aggregate by semana or by mes
serverEssaSemana<- function(id, dir_data, db_emprendedoras, periodo = "Semana") {
  moduleServer(id, function(input, output, session) {
    
    
#look up emprendedoras----------------------------------------------------------
    dir_lookups <- file.path(dir_data,"0look_ups")
    emprendedoras <- db_emprendedoras
    
   
    
#load presencas and define period (either month or week)-----------------------
    presencas <- reactive({
      
      #create data of presencas
      #imports clean/all_presencas.rds
      #keeps only status == Presente, creates month, names all modulos as modulos obligatorios
      presencas <- create_data_presencas(dir_lookups, dir_data)
     
    #define periodo
    #renmaes the variable week or month as periodo so it can be aggregated later
    presencas <- define_var_periodo(presencas, periodo)
     
     presencas
     
     })
     
    

#dynamically update choices for input$semanas ----------------------------------
    observe({
      
      #get semanas reported
      semanas <- sort(unique(presencas()$periodo), decreasing = T)
      message(names(presencas()))
      print(unique(presencas()$periodo))
      updateSelectInput(session,
                        "periodo",
                        choices = semanas)
                        
      
      
    })

    
#reactive data for period ------------------------------------------------------

    this_period <- reactive({
      
      presencas() %>%
        filter(periodo == input$periodo)
      
    })
    
#data that counts participacoes by periodo -------------------------------------  
    data_user <- reactive({
      
      #run function create_data_week, based on the selection of the user
      data_for_this_week <- list(`Seu todo` = create_data_week(this_period(), todos = T), 
                                 `Por Cidade` = create_data_week(this_period(), F, by = Cidade),
                                 `Por Abordagem` = create_data_week(this_period(), F, by = Abordagem),
                                 `Por Cidade e Abordagem` = create_data_week(this_period(), F, by = c("Abordagem", "Cidade"), double_group = T),
                                 `Por Actividade no seu todo` = create_data_week(this_period(), F, by = actividade),
                                 `Por Actividade por Abordagem` = create_data_week(this_period(), F, by = c("actividade", "Abordagem"), double_group = T), 
                                 `Por actividade por cidade` = create_data_week(this_period(), F, by = c("actividade", "Cidade"), double_group = T)
      )
      
      #fetch data base that user selected
      db <- data_for_this_week[[input$by]]
      
      
      #Set names of data_plot based on type of plot
      #I am doing this so all the names are consistent for plotting
      cuantos_names <- length(names(db))
      if(cuantos_names==3){
        
        names(db) <- c("target", "facet", "value")
      }
      
      db
      
    })
    
   
   
    
    
     
#Get first and last day of the period -----------------------------------------------------------------
  
    first_day <- reactive({min(this_period()$data_posix)})
    last_day <- reactive({max(this_period()$data_posix)})
    
# Identify the number of variables in the data----------------------------------
# if there are 3 it is because it was grouped by 2 variables
    
    cuantos_names <- reactive({
      
      length(names(data_user()))
    })
    
    
    
#get figures of total emprendedoras by Cidade, Abordagem, or both -------------
#this is used to plot and compare participation against targets
#it creates a dataset with three names:
#target (or grouping variable)
# total : total emprendedoras na listas de BM
# interesadas: total emprendedoras interesadas
# facet: variable to facet the plot
    data_totais <- reactive({
      
      create_data_totais(db_emprendedoras, input$by)
      
    })
    
    
    
    
#Plot the data -----------------------------------------------------------------
    output$plot <- renderPlot({
      
      
      base_plot <- data_user() %>%
        ggplot(aes(x = target,
                   y = value,
                   )
               ) 
      
      #if it is a facet
      if(cuantos_names() ==3){
        
        plot <- base_plot +
          geom_point(size = 5,
                     shape = 21,
                     aes(fill = target)) +
          facet_wrap(~ facet)
      #elese
        } else {
        
        plot <- base_plot +
          geom_point(size = 5,
                     shape = 21,
                     aes(fill = target)
          )
        }
      
      #add targets to check if participation is higher or lower than expeceted =
      if(input$by %in% c("Seu todo", "Por Cidade", "Por Abordagem",
                         "Por Cidade e Abordagem" )){
        
        plot <- plot +
          geom_point(data = data_totais(),
                     aes(x = target,
                         y = total,
                         color = "Na lista de BM"
                         ),
                     shape = 0,
                     size = 4
          ) +
          geom_point(data = data_totais(),
                     aes(x = target,
                         y = interesadas,
                         color = "Interesadas"
                        ),
                    
                     fill = NA,
                     shape = 2,
                     size = 4
                    
          ) +
          scale_color_manual(values = c("green", "black"), 
                            guide = guide_legend(override.aes = list(shape = c(2,0))))
                     
      }
      
      plot +
        expand_limits(y = 0) +
        scale_fill_manual(values = palette)+
          labs(
            y = "Número de emprendedoras",
            x = ""
          ) +
        theme_realiza()
          # theme(axis.ticks = element_blank(),
          #       axis.title = element_text(size = 16),
          #       axis.title.y = element_text(margin = margin(r = 10)),
          #       axis.text = element_text(size = 10),
          #       axis.text.x = element_text(angle = 90),
          #       plot.background = element_blank(),
          #       panel.background = element_blank(),
          #       panel.grid.minor.y =  element_line(linetype = "dotted", color = "gray"),
          #       panel.grid.major.y =  element_line(linetype = "dotted", color = "gray"),
          #       legend.title = element_blank(),
          #       legend.position = "top",
          #       legend.text = element_text(size = 12),
          #       legend.key = element_rect(fill = NA)
          # )
      
    })
    
    
    
    
    output$header <- renderUI({

      HTML(
        glue("<h5>Os gráficos mostram o número de total emprendedores que participaram das atividades da Realiza durante
             <b>{first_day()} e {last_day()}</b>. </h5>")

      )
    })
    
    
    observeEvent(input$by,{
      
      print(input$by)
      
    })
    
    
  })
  
  
}
