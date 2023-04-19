library(ggplot2)
ui_summary <- function(id){
  
  
  tagList(
    
    sidebarLayout(
      
      sidebarPanel(width = 4,
                   selectInput(NS(id,"by"), 
                               label = "Desagregação",
                               choices = setNames(
                                 c("Cidade", "Grupo"),
                                 c("Cidade", "Grupo")
                                
                               )),
                   
                   
                   checkboxInput(NS(id,"by_time"), label = "Ao longo do tempo", value = TRUE),
                   ),
      mainPanel(
        uiOutput(NS(id,"header_confirmadas")),
        plotOutput(NS(id,"plot"))
        
      )
    )
    
  )
}


#Server ===================================================================

#Server ======================================================================
serverSummary<- function(id, dir_data) {
  moduleServer(id, function(input, output, session) {
    
    
    data_summary <- rio::import(file.path(dir_data,"1.zoho/3.clean/all_presencas.rds")) %>%
      dplyr::filter(status_realiza == "CONFIRMADA")
    
  
    confirmadas <- length(unique(data_summary$Emprendedora))
    
    data_plot <- reactive({
      
      #if user wants by time
      if(input$by_time){
        
        db <- data_summary %>%
          group_by_at(c(input$by, "week")) %>%
          summarise(mean_presenca = mean(presente, na.rm = T), .groups = 'drop')
      } else {
        
        db<- data_summary %>%
          group_by_at(input$by) %>%
          summarise(mean_presenca = mean(presente, na.rm = T), .groups = 'drop')
        
      }
      
      db %>%
        rename(target = input$by)
      
      
    })
    
    
    
    output$header_confirmadas <- renderUI({
      
      HTML(
        glue("<h3>Os gráficos mostram a taxa de frequência das {confirmadas} participantes CONFIRMADAS pelo programa </h3>")
      
      )
        })
    
    output$plot <- renderPlot({
      
      
      if(input$by_time){
        
        data_plot() %>%
          mutate(week = week - (min(week) - 1)) %>%
          ggplot(
            aes(x = week,
                y = mean_presenca,
                color = target
                )
          ) +
          
          geom_line(size = 2) +
          labs(y = "Presenças (%)",
               x = "Semana de implementação" 
          ) +
          scale_y_continuous(labels = function(x)x*100) +
          scale_color_manual(values = c("#A45EDA", "#F77333", "#5DD4C8"))+
          theme(axis.ticks = element_blank(),
                axis.title = element_text(size = 20),
                axis.title.y = element_text(margin = margin(r = 10)),
                axis.text = element_text(size = 16),
                plot.background = element_blank(),
                panel.background = element_blank(),
                panel.grid.minor.y =  element_line(linetype = "dotted", color = "gray"),
                panel.grid.major.y =  element_line(linetype = "dotted", color = "gray")
          ) +
          theme(legend.title = element_blank(),
                legend.position = "top",
                legend.text = element_text(size = 12),
                legend.key = element_rect(fill = NA)
                )
      } else {
        
        
        data_plot() %>%
          ggplot(
            aes(x = target,
                y = mean_presenca,
                label = paste0(round(mean_presenca * 100,0), "%")
                )
          ) +
          geom_point(size = 6,
                     color = "#EF6F31") +
          labs(y = "Presenças (%)",
               x = " " 
               ) +
          geom_text(hjust = -.5)+
          scale_y_continuous(labels = function(x)x*100) +
          theme(axis.ticks = element_blank(),
                axis.title = element_text(size = 20),
                axis.title.y = element_text(margin = margin(r = 10)),
                axis.text = element_text(size = 16),
                axis.text.x = element_text(angle = 90),
                
                plot.background = element_blank(),
                panel.background = element_blank(),
                panel.grid.minor.y =  element_line(linetype = "dotted", color = "gray"),
                panel.grid.major.y =  element_line(linetype = "dotted", color = "gray")
                
                )
      }
      
      
    })
    
    
    observeEvent(input$by,{
      
      print(input$by)
      
    })
    
    
  })
  
  
  }
    