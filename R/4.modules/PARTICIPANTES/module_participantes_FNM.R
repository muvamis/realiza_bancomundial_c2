ui_participantes_FNM <- function(id){
  
  
  
  tagList(
    sidebarLayout(
      
      sidebarPanel(
        width = 2,
        selectInput(NS(id,"cidade"), 
                    label = h4("cidade"),
                    #the choices for periodo are defined in 1.Utils-app/filtro_periodo.R
                    choices = "All"),
        selectInput(NS(id,"mes"), 
                    label = h4("Mes"),
                    #the choices for periodo are defined in 1.Utils-app/filtro_periodo.R
                    choices = "All")
        
        
      ),
      
      mainPanel(
        
        fluidRow(
          h4(
            HTML("O Banco seleccionou 350 mulheres para participar da abordagem conecta. <br><br>
             O gráfico da esquerda mostra o número de atividades realizadas e o da direita
         o numero de mulheres agendadas e que participaram das actividades.")
         )
         
        ),
        br(),
        
        fluidRow(
          
          
          
          column(width = 6,
                 mainPanel(
                   withSpinner(plotlyOutput(NS(id,"plot_actividades"), width = '450px'), color = "black")
                 )
          ),
          column(width = 6,
                 mainPanel(
                   withSpinner(plotlyOutput(NS(id,"plot_emprendedoras"), width =  "500px"), color = "black")
                 )
          )
        ),
        
        br(), hr(),
        
        fluidRow(
          h2(
            HTML("Eventos realizados no periodo seleccionado:")
          )
          ),
        br(),
        fluidRow(
          
          DT::DTOutput(NS(id, 'table'))
        )
      )
      
      
    )
    
    
    
    
  )
}

#server 

server_participantes_FNM <- function(id, db_emprendedoras, db_presencas
){
  
  moduleServer(id, function(input, output, session){
    
    #identify_grupo() is defined in 1.Utils-app/identify-grupo.R
    #it detects the name of the grupo in the id of the module and returns a character
    #with the name as it is in the data (FNM or SGR or FNM + SGR)
    grupo_modulo <- identify_grupo(id)
    
    start <- reactive({1})
    
    
    #Data for this module ===========================================================
    #presencas_de_grupo() is created un 0.utils-clean-data/presencas_de_grupo.R
    #it keeps the data for the given grupo, removes certain actividades that are 
    #not of interest for this analysis and keeps the given status.
    data_module <- reactive({presencas_de_grupo(presencas_db = db_presencas,
                                      grupo = grupo_modulo,
                                      #activities_sgr is created in 0.utils-clean-data/vector-actividades
                                      avoid_actividade = activities_sgr,
                                      keep = c("Presente", "Ausente", "Pendente")
    ) %>% mutate(
      
      actividade = factor(actividade,
                          #activities_fnm is created in 0.utils-clean-data/vector-actividades
                          levels= activities_fnm,
                          ordered = T),
      actividade =recode_fnm(actividade) 
    ) %>%
      #keep only events that have ocurred so far
      group_by(Nome_do_evento, data_evento) 
      
      })
    
    
    
    #Update filtro of the cidade 
    observe({
      
      
      y <- as.character(unique(data_module()$Cidade))
      
      # Can use character(0) to remove all choices
      if (is.null(y))
        y <- character(0)
      
      # Can also set the label and select items
      updateSelectInput(session, "cidade",
                        choices = c("All",y)
      )
      
      
    })
    
    
    
    data_cidade <- reactive({
      
      if(input$cidade == "All"){
        
        d <- data_module()
      } else {
        
        d <- data_module() %>%
          filter(Cidade == input$cidade)
      }
      
      d
      
    })
    
    
    
    #Update filtro of the month 
    observe({
     
      
      y <- as.character(unique(data_cidade()$mes))
      
      # Can use character(0) to remove all choices
      if (is.null(y))
        y <- character(0)
      
      # Can also set the label and select items
      updateSelectInput(session, "mes",
                        choices = c("All",y)
      )
      
      
    })
    
    
    
    #reactive data period ======================================================
    
    data_period <- reactive({
      
      if(input$mes == "All"){
        
        d <- data_cidade()
      } else {
        
        d <- data_cidade() %>%
          filter(between(mes, min(mes), input$mes))
        
        
      }
      
      d
      
    })
    
    
    
    #reactive data actividades ==============================================================
    
    
    data_plot_actividades <- reactive({
      
      data_period() %>%
        #keep only one record for each evento
        group_by(actividade, Nome_do_evento, data_posix) %>%
        slice(1) %>%
        ungroup() %>%
        #count how many eventos of each actividade were conducted
        group_by(actividade) %>%
        summarise(agendadas = n(), .groups = 'drop')
      
      
      
    })
    
    
    
    #reactive texto perido 
    
    text_periodo <- reactive({
      if(input$mes == "All"){
        
        durante = 'até agora'
      } else {
        
        durante = glue('até o mês {input$mes} ')
      }
      
      durante
      
    })
    
    #plot
    output$plot_actividades <- renderPlotly({
      
      
      plot <- data_plot_actividades() %>%
        filter(actividade != "Individuais") %>%
        ggplot(aes(x = actividade,
                   y = agendadas,
                   label = agendadas)) +
        geom_col(width = .7,
                 fill = palette[3]) +
        geom_text()+
        labs(y = 'Numero de actividades',
             x = "",
             title = glue("Actividades realizadas {text_periodo()}")
             ) +
        theme_realiza() +
        theme(axis.text.x = element_text(angle = 45))
      
      
      ggplotly(plot) %>%
        config(displayModeBar = F)
      
    })    
    
    
    #reactive data emprendedoras ===================================================
    
    data_plot_emprendedoras <- reactive({
      data_period() %>%
        group_by(actividade) %>%
        summarise(Agendadas = n(),
                  Presentes = sum(presente, na.rm = T),
                  .groups = 'drop'
        ) %>%
        pivot_longer(-actividade,
                     names_to = "status",
                     values_to = "total")
      
      
    })
    
    
    #plot 
    output$plot_emprendedoras <- renderPlotly({
      
      # print(unique(data_plot_emprendedoras()$actividade))
      
      plot <- data_plot_emprendedoras() %>%
        ggplot(aes(x = actividade,
                   y = total,
                   fill = status,
                   label = total)) +
        geom_col(width = .7,
                 position = 'dodge2') +
        scale_fill_manual(values = palette) +
        labs(y = "Numero de Mulheres",
             x = "",
             title = glue("Agendadas vs Presentes {text_periodo()}")
        ) +
        geom_text(position = position_dodge(0.7),
                  vjust = -1,
                  size = 3
                  )+
        
        theme_realiza() +
        theme(axis.text.x = element_text(angle = 45))
      
      
      ggplotly(plot) %>%
        config(displayModeBar = F)
      
      
      
    })
    
    #table
    output$table <- DT::renderDataTable({
     
      data_period() %>%
        filter(actividade != "Individuais") %>%
        group_by(Cidade,data_evento, actividade, Nome_do_evento) %>%
        summarise(Agendadas = sum(agendada),
                  Presentes = sum(presente),
                  .groups = 'drop') %>%
        rename(`Data evento` = data_evento,
               `Evento` = actividade,
               `Nome do evento` = Nome_do_evento
               )
      
    }, rownames= FALSE,
    options =list(
      #pt is created in R/utils-app/language_DT.R
      language = pt
      )
    )
    
    
    
    
    
    
    
  })
  
}
