library(DT)

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
ui_metas_fnm <- function(id){
  
  
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
        p("Para cada tipo de atividade de FNM, espera-se que cada emprendedora compareça a um número de sessões obrigatórias.
          O número de sessões obrigatórias que as mulheres devem participar para cada atividade é o seguinte"),
     
        
        DT::DTOutput(NS(id,"table_actividades")),
        
        br(),br(),
        
        uiOutput(NS(id,"header")),
        
        br(),
        
        withSpinner(plotlyOutput(NS(id,"plot")), color = "black")
        
      )
    )
    
  )
}



#Server ======================================================================

#'@param periodo c("Semana", "Mes") defines whether to aggregate by semana or by mes
serverMetasFNM<- function(id, dir_data, db_emprendedoras) {
  moduleServer(id, function(input, output, session) {
    
    #define palete for chart ----------------------------------------------------
    gray<-"#F5F5F5"
    completed <- "#2A6B94"
    moreHalf <- "#61C2B1"
    lesHalf <- "#AAF2BB"
    
    
    #define categories of progress ---------------------------------------------
    
    
    categories <- reactive({
      c("Ainda não participou","Participou em MENOS da metade das sessões",
                    "Participou em MAIS de metade das sessões", "Completou todas as sessões")
    })
    
    
    #read lookup de actividades ------------------------------------------------
    dir_lookups <- file.path(dir_data,"0look_ups")
    
    actividades <- import(file.path(dir_lookups,"actividades.rds")) %>%
      select(actividade, sessoes) %>%
      distinct()
    
    
    output$table_actividades <- DT::renderDataTable({
      
      datatable(
        actividades %>%
          rename(
            Actividade = actividade,
            `Sessoes obrigatórias`= sessoes
            
          ),
        rownames = F,
        options = list(dom = 't')
      ) 
      
    })
    
    
    
    #load presencas and define period (either month or week)-----------------------
    fnm <- reactive({
      
      print(categories())
     
      #create data of presencas
      #imports clean/all_presencas.rds
      #keeps all status(Presente, Ausente y Pendente), creates month, names all modulos as modulos obligatorios
      presencas <- create_data_presencas(dir_lookups, dir_data, c("Presente") )
      #Track progress of emprendedoras agains targets
      
      print(tabyl(presencas, actividade))
      fnm <- presencas %>%
        #remove modulos (because it is SGR)
        filter(actividade_label != "Modulos Obligatorios",
               Abordagem != "SGR") %>%
        #Count presencas by actividade
        group_by(ID_BM,Cidade,Abordagem ,actividade) %>%
        summarise(presente = sum(presente),
                  .groups = 'drop') %>%
        #join with actividades to get the number of mandatory sessions
        left_join(actividades) %>%
        #Estimate level of progress by emprendedora and actividade
        mutate(completas = presente/as.numeric(sessoes) * 100,
               #Create groups of progress
               progress = case_when(between(completas,0,49) ~ categories()[2],
                                    between(completas, 50,99) ~ categories()[3],
                                    completas > 99 ~ categories()[4]
                                    
               ) 
        ) %>%
        #Count by cidade
        group_by(Cidade, Abordagem,actividade, progress) %>%
        summarise(mulheres = n(),
                  .groups = 'drop')
      
    
      
      fnm
      
    })
    
    
    # #create data for plot
    data_plot <- reactive({

      db <- create_data_progress_FNM(fnm(),db_emprendedoras, input$by, cats=categories())

      if(input$indicador == "bm"){

        db <- db %>% rename(value = prop_wb)

      } else {

        db <- db %>% rename(value = prop_int) %>% mutate(total = interesadas)     }

      db %>%
        mutate(Emprendedoras = glue("{mulheres} <br>
                                    <b>que {progress}</b>
                                    das {total} nas {text_header()}
                                    "))
    })
    
   
    
    
    
     text_header <- reactive({
    
    if(input$indicador == "bm"){
      
      text = "listas do Banco Mundial" 
      
    } else {
      
      text = "listas das emprendedoras interesadas en participar" 
    }
    
    text
  })
    
#identify if the data is by todo or not
#if it is by todo, the chart wont facet
    todo <- reactive({
      
      data_plot()$target[1] == "Seu todo"
      
    })
    
    
    
    
    
    
    
    
    
    #Plot the data -----------------------------------------------------------------
    output$plot<- renderPlotly({
      

      plot <- data_plot() %>%
        ggplot(aes
               (x = value,
                 y = actividade,
                 fill = progress,
                 label = Emprendedoras
               )
        ) +
        geom_col() +
        geom_vline(xintercept = .5)+
        labs(y = "",
             x = "Proporção de emprendedoras")+
        scale_fill_manual(values = c(gray, lesHalf, moreHalf, completed),
                          breaks = categories(),
                          name = "") +
        scale_x_continuous(labels = function(x){paste0(x*100, "%")},
                           position = "top"
                           ) +
        guides(fill = guide_legend(nrow = 2)) +
        theme_realiza()+
        theme(legend.text = element_text(size = 8),
              axis.text.y = element_text(hjust = 0),
              axis.text = element_text(size = 10),
              axis.title = element_text(size = 12)) 
      
      #facet if neeeded
      if(!todo()){
        
        plot <- plot +
          facet_wrap(~target)
      }
      
      
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
           O gráfico mostra a percentagem de mulheres que participaram em menos de metade das
             sessões obrigatórias, em mais de metade e em todas as sessões obrigatórias.
           <b> em relação às {text_header()}</b>. </p>"
           )
        
      )
    })
    
    observeEvent(input$by,{
      
      print(input$by)
      
    })
    
    
  })
  
  
}
