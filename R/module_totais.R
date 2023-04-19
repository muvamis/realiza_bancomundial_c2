library(rio)
library(janitor)
library(tidyr)
library(ggplot2)

selections <- setNames(
  #values
  c("Status", "Cidade", "Componente", "Abordagem", "Cidade_Abordagem"),
  #labels
  c("Seu Todo", "Por Cidade", "Por Componente", "Por Abordagem", "Por Cidade e Abordagem")
  
)

ui_totals <- function(id){
  
  
  tagList(
    
    sidebarLayout(
      
      sidebarPanel(width = 3,
                   selectInput(NS(id,"by"), 
                               label = h4("Números da operação por:"),
                               choices = selections
                               
                   ),
                   shiny::checkboxGroupInput(NS(id,"check_group"),
                                             label = h4("Status"),
                                             choices = list(
                                               
                                               "Nas Listas BM" = 1,
                                                "Interesadas em participar" = 2,
                                                "Tem participado" = 3,
                                                "Veio sessao inaugural" =4 
                                               ),
                                             selected = c(1,2,3,4)
                                               
                                             )
                                             
                                             
      ),
      mainPanel(
        uiOutput(NS(id,"header")),
        withSpinner(plotOutput(NS(id,"plot")), color = "black")
        
      )
    )
    
  )
}


#Server ===================================================================

#Server ======================================================================
serverTotals<- function(id, dir_data) {
  moduleServer(id, function(input, output, session) {
    
    
    ## read look up of emprendedoras
    emprendedoras <- import(file.path(dir_data,"0look_ups/emprendedoras.rds"))
    presencas <- import(file.path(dir_data,"1.zoho/3.clean/all_presencas.rds")) %>%
      filter(Status == "Presente")
    

    # all emprendedoras nas listas do banco mundial
    listas_BM <- emprendedoras %>%
      mutate(interesada = status_realiza == "CONFIRMADA",
             nas_listas = TRUE,
             across(c(interesada, nas_listas), function(x)ifelse(x, 1,0))) %>%
      select(ID_BM, Cidade, 
             Abordagem = grupo_accronym,
             interesada,
             nas_listas
      )
    
    
    #participations
    participants <- presencas %>%
      mutate(inaugural = actividade =="Sessão Inaugural") %>%
      group_by(ID_BM) %>%
      summarise(inaugural = max(inaugural),
                participado = 1,
                .groups = 'drop') 
    
    
    #join both
    data_totais <- listas_BM %>%
      left_join(participants, by = "ID_BM") %>%
      #rename for presentation
      rename(`Nas Listas BM` = nas_listas,
             `Interesadas em participar` = interesada,
             `Tem participado` = participado,
             `Veio sessao inaugural` = inaugural
      ) %>%
      select(-ID_BM) %>%
      #reshape for visuals
      pivot_longer(-c(Abordagem, Cidade),
                   names_to = "Status") %>%
      mutate(Status = factor(Status,
                             levels = c("Nas Listas BM",
                                        "Interesadas em participar",
                                        "Tem participado",
                                        "Veio sessao inaugural"
                                        
                             )))
    names(data_totais)      
    
    # 
    # ##identify emprendedoras that attended to the first session
    # inaugural<-  presencas %>% 
    #   filter(actividade =="Sessão Inaugural") %>%
    #   select(ID_BM, Status)
    # 
    # 
    # ##identify emprendedoras that attended to the primera sessao
    # primera<- presencas %>%
    #   group_by(ID_BM) %>%
    #   #filter(data_posix == min(data_posix)) %>%
    #   slice(1) %>%
    #   ungroup() %>%
    #   mutate(actividade = "Primera sessao") %>%
    #   select(ID_BM, Status_primera = Status)
    # 
    # 
    # 
    # ## Join lookup of emprendedoras with peresencas of first session
    # data_totais <- emprendedoras %>%
    #   select(ID_BM, Cidade, 
    #          Abordagem = grupo_accronym,
    #          status_realiza) %>%
    #   left_join(inaugural, by = "ID_BM") %>%
    #   left_join(primera, by = "ID_BM") %>%
    #   group_by(Abordagem, Cidade) %>%
    #   ##Count total in WB data, Confirmadas, and those who attended the first session
    #   summarise("Nas Listas BM" = n(),
    #             `Interesadas em participar` = sum(status_realiza == "CONFIRMADA", na.rm = T),
    #             `Tem participado` = sum(!is.na(Status_primera)),
    #             `Veio sessao inaugural` = sum(!is.na(Status)),
    #             
    #             .groups = 'drop'
    #   ) %>%
    #   pivot_longer(-c(Abordagem, Cidade),
    #                names_to = "Status") %>%
    #   mutate(Status = factor(Status,
    #                          levels = c("Nas Listas BM",
    #                                     "Interesadas em participar",
    #                                     "Tem participado",
    #                                     "Veio sessao inaugural"
    #                                    
    #                          )))
    
    
    
    status_vector <-  c("Nas Listas BM",
                        "Interesadas em participar",
                        "Tem participado",
                        "Veio sessao inaugural")
      
      
      
    
    
    # reactive data -----------------------------------------------------------
    data_plot <- reactive({
      
      
      
      if(input$by == "Cidade_Abordagem"){
        
        agrupar_por <- c("Cidade", "Abordagem", "Status")
        
        
      } else if (input$by == "Componente"){
        
        
        sgr <- data_totais %>% 
          dplyr::filter(str_detect(Abordagem, "SGR")) %>%
          mutate(Componente = "SGR")
        
        
        fnm <- data_totais  %>% 
          dplyr::filter(str_detect(Abordagem, "FNM")) %>%
          mutate(Componente = "FNM")
        
        data_totais <- rbind(sgr, fnm)
        
        print(names(data_totais))
        
        agrupar_por <- c("Componente", "Status")
        
        
      } else if (input$by %in% c("Cidade", "Abordagem")){
        
        agrupar_por <- c(input$by, "Status")
      } else {
        
        agrupar_por <- input$by
      }
      
      
      data_plot <- data_totais %>%
        group_by_at(agrupar_por) %>%
        summarise(value = sum(value, na.rm = T),
                  .groups = 'drop') %>%
       dplyr::filter(Status %in% status_vector[as.numeric(input$check_group)])
      
      
    })
    
    
    
    output$header <- renderUI({
      
      tags$div(
        p("O Banco Mundial compartilhou uma lista com informações sobre mulheres que foram
esperado para participar do programa. Com essas informações, a equipe MUVA
entrei em contato com todas as mulheres para confirmar que ainda estavam interessadas em participar.
"),
br("O gráfico a seguir mostra:"),
tags$ul(
  
  tags$li("Total de mulheres incluídas nas listas do banco mundial,"),
  tags$li("Total de mulheres que confirmaram à equipe MUVA que ainda estavam interessadas em participar,"),
  tags$li("Total de mulheres que participaram de qualquer atividade do programa",),
  tags$li("Total de mulheres que participaram da sessão inaugural.")
  
)





      )
      
      
      
      
    })
    
    output$plot <- renderPlot({
      
      print(names(data_plot()))
      upper_limit = max(data_plot()$value)
      
      #if it is seu todo
      if(input$by == "Status") {
        
        base_plot <- data_plot() %>% 
          ggplot(aes(x = "",
                     y = value,
                     fill = Status,
                     label = value,
                     color = Status)
          )
        
      } else if (input$by %in% c("Cidade")){
        
        base_plot <- data_plot() %>% 
          ggplot(aes(x = Cidade,
                     y = value,
                     fill = Status,
                     label = value,
                     color = Status)
          )
        
        
      } else if (input$by == "Componente"){
        
        base_plot <- data_plot() %>% 
          ggplot(aes(x = Componente,
                     y = value,
                     fill = Status,
                     label = value,
                     color = Status)
          )
        
        
      } else if (input$by %in% c("Abordagem","Cidade_Abordagem")){
        
        base_plot <- data_plot() %>% 
          ggplot(aes(x = Abordagem,
                     y = value,
                     fill = Status,
                     label = value,
                     color = Status)
          )
        
        
      }
      
      plot <- base_plot +
        geom_point(size = 6,
                   shape = 21) +
        geom_text(hjust = -.5) 
      
      if(input$by == "Cidade_Abordagem"){
        
        plot <- plot +
          facet_wrap(~Cidade,
                     ncol = 3)
        
        
        
      }
      
      #Define theme
      plot +
        scale_y_continuous(limits = c(0,upper_limit)) +
        scale_color_manual(values = c(palette))+
        scale_fill_manual(values = c(palette))+
        labs(
          y = "Número de emprendedoras",
          x = ""
        ) +
        theme_realiza()
      
      
      
    })
    
    
    observeEvent(input$by,{
      
      print(input$by)
      
    })
    
    
  })
  
  
}
