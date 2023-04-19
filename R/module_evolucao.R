library(rio)
library(janitor)
library(tidyr)
library(ggplot2)
library(plotly)
library(shinycssloaders)


selections_semana <- setNames(
  #values
  c("Seu todo" ,
    "Por Cidade", 
    "Por Abordagem", 
    "Por Cidade e Abordagem"
    #"Por Actividade no seu todo", 
    #"Por Actividade por Abordagem",  
    #"Por actividade por cidade" 
  ),
  #labels
  c("Seu todo" ,
    "Por Cidade", 
    "Por Abordagem", 
    "Por Cidade e Abordagem"
    #"Por Actividade no seu todo", 
    #"Por Actividade por Abordagem",  
    #"Por actividade por cidade" 
  )
  
)

#UI ===========================================================================
#'@param periodo c("Semana", "Mes") it defines the labels of the selectors
ui_evolucao <- function(id, periodo = "Semana"){
  
  
  tagList(
    
    sidebarLayout(
      
      sidebarPanel(width = 3,
                   selectInput(NS(id,"by"), 
                               label = h4("Números da operação por:"),
                               choices = selections_semana
                               
                   ),
                   selectInput(NS(id,"indicador"), 
                               label = h4("Em relação ao:"),
                               choices = setNames(c("bm", "interesadas"), c("Listas de BM", "Interesadas"))
                               
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
serverEvolucao<- function(id, dir_data, db_emprendedoras, periodo = "Semana") {
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
    
    
    
    #get presencas over period -----------------------------------------------------
    
    
    
    
    data_evolucao <- reactive({
      
      create_data_evolucao(presencas(), input$by)
      
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
    
    data_evolucao_perc <- reactive({
      
      db <- data_evolucao() %>%
        left_join(data_totais()) %>%
        mutate(presencas_bm = presentes/total,
               presencas_interesadas = presentes/interesadas) %>%
        pivot_longer(c(presencas_bm, presencas_interesadas),
                     names_to = "Indicador")
      
      if(periodo == "Semana"){
        
        db <- db %>% mutate(periodo = factor(as.character(periodo),
                                             labels = as.character(seq(1,max(db$periodo),1)),
                                             ordered = T
        )
        )
      } 
      
      if(input$indicador == "bm"){
        
        db <- db %>% filter(Indicador == "presencas_bm" )
      } else {
        
        db <- db %>% filter(Indicador == "presencas_interesadas" ) %>%
          mutate(total = interesadas)
      }
      
      db %>%
        mutate(Participantes = glue("{presentes} 
                                    Total nas {text_header()}: {total}
                                    Taxa de participacao: {paste0(round(value * 100,1),'%')}"
                                    )
               )
      
    })
    # 
    
    
    # observe(
    #   print(data_evolucao_perc())
    #   
    # )
    
    
    
    # Identify the number of variables in the data----------------------------------
    # if there are 3 it is because it was grouped by 2 variables
    
    cuantos_names <- reactive({
      
      length(names(data_evolucao_perc()))
    })
    
    
    
    
    
    
    
    #Plot the data -----------------------------------------------------------------
    output$plot <- renderPlotly({
      
      
      base_plot <- data_evolucao_perc() %>%
        ggplot(aes(x = periodo,
                   y = value,
                   color = target,
                   group = target,
                   label = Participantes
        )
        
        
        ) +
        geom_point() +
        
        geom_line(size = 1)
      
      
      
      #if it is a facet
      if(cuantos_names() ==9){
        
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
          y = "Taxa de participacao",
          x = periodo
        ) +
        theme_realiza() +
        scale_color_manual(values = c(palette),
                           name = "") +
        scale_y_continuous(labels = function(x){paste0(x*100, "%")},
                           limits = c(0,1.2)
                           )
      
      
      ggplotly(final_plot,
               tooltip = c("label")) %>% 
        config(displayModeBar = F)
      
    })
    
    
    text_header <- reactive({
      
      if(input$indicador == "bm"){
        
        text = "listas do Banco Mundial" 
        
      } else {
        
        text = "listas das emprendedoras interesadas en participar" 
      }
      
      text
    })
    
    
    output$header <- renderUI({
      
      HTML(
        
        glue("
            <p>
             Para conhecer o nível de participação, o número de mulheres 
             que participaram <b>em cada {periodo}</b> desde o início do programa 
             é monitorado.
             </p>
             <p>Os gráficos mostram a proporção de mulheres empreendedoras 
             que participaram do programa durante cada {periodo} em <b>relação às {text_header()}
             </b>. 
             </p>")
        
      )
    })
    
    
    observeEvent(input$by,{
      
      print(input$by)
      
    })
    
    
  })
  
  
}
