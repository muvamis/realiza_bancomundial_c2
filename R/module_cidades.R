#Module sessoes
library(dplyr)
library(ggplot2)

ui_cidades <- function(id){
  
  tagList(
    
    
    sidebarLayout(
      sidebarPanel(width = 2,
                   #Inputs Cidade
                   selectInput(NS(id,"cidades"), "Cidade",
                               c("Beira", "Maputo", "Nampula")
                   )
                  
      ),
      mainPanel(width = 10,
                #Header of Agente (name and % of assistance)
                uiOutput(NS(id,"header")),
                #table with presences
                withSpinner(plotOutput(NS(id,"plot")))
      )
    )
    
    
    
  )
  
}



#Server ======================================================================
serverCidade <- function(id, grupo, dir_data) {
  moduleServer(id, function(input, output, session) {
    
    if(grupo %in% c("sgr", "fnm")){
      
      infile_stats <- glue::glue("{dir_data}/2.Dashboard/{grupo}_stats.rds") 
      data_stats <- rio::import(infile_stats) %>% dplyr::filter(grupo_accronym == define_accronym(grupo))
      
    } else {
      
      infile_stats_sgr <- glue::glue("{dir_data}/2.Dashboard/sgr_stats.rds") 
      infile_stats_fnm <- glue::glue("{dir_data}/2.Dashboard/fnm_stats.rds") 
      
      data_stats_fnm <- rio::import(infile_stats_fnm) %>% dplyr::filter(grupo_accronym == define_accronym(grupo))
      data_stats_sgr <- rio::import(infile_stats_sgr) %>% dplyr::filter(grupo_accronym == define_accronym(grupo))
      
      data_stats <- plyr::rbind.fill(data_stats_sgr, data_stats_fnm)
    }
   
    
    #Reactive elements ========================================================
    
    
    confirmadas <- reactive({
      
      length(unique(
      data_stats$Emprendedora[data_stats$status_realiza == "CONFIRMADA" & data_stats$Cidade == input$cidades]
      ))
      
    })      
    
    #react to selected cidade
    data_cidade <- reactive({
      
      data_stats %>%
        dplyr::filter(Cidade == input$cidades,
                      status_realiza == "CONFIRMADA") %>%
        group_by(actividade_label) %>%
        avg_presencas()
    })
    
   
    #Summary of all cidades
    data_all <- data_stats %>%
      group_by(actividade_label) %>%
     avg_presencas()
    
    
    
    
    #Main page ================================================================
    
    
    g <- 
      if(grupo == "sgr"){
        
        "SGR"
      } else if(grupo == "fnm"){
        
        "FNM"
      } else {
        
        "FNM + SGR"
      }
    
    
    
    output$header <- renderUI({
      #get average of presencas of this agente
      avg <- scales::percent(mean(data_cidade()$presencas_avg_num, na.rm =T))
      
      tags$div(
      h1(glue("{input$cidades} - {g}")),
      h2(glue("{confirmadas()} emprendedoras confirmadas")),
      h3(paste0("Taxa de presenças (das confirmadas): ", avg))
     
     
      )
      
    })
    
    output$plot <- renderPlot({
      
      data_cidade() %>%
        ggplot(aes(x = actividade_label,
                   y = presencas_avg_num)
               
               ) +
        geom_col(width = .7,
                fill = "#63CAC8"
                 ) +
        
        geom_point(data = data_all,
                   aes(x = actividade_label,
                       y = presencas_avg_num,
                       fill = "blue"
                       
                      ),
                  
                   shape = 21,
                   size = 5
        ) +
        labs(y = "Presenças (%)",
             x = "" 
        ) +
        scale_fill_manual(values = c("#F77333", "blue"),
                          labels = c("Taixa presencas presença de todas as cidades", "Cidade")) +
        # scale_fill_manual(name = "Avg.Realiza (tudas cidades)",
        #                   breaks = "Avg.Realiza (tudas cidades)",
        #                   values = c("blue")) +
        scale_y_continuous(labels = function(x){x*100}) +
        
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
      
    })
    
    
    
 
    
  })
}