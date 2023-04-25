#library(shiny)

#UI ---------------------------------------------------------------------------
ui_sessoes_obrigatorias<- function(id, mode = "tabela"){
  
  
  
  tagList(
    
    #intro text
    fluidRow(
      shiny::uiOutput(NS(id, "intro"))
    ),
    
    br(),
    
    sidebarLayout(
      
      sidebarPanel(
        
        width = 2,
        selectInput(NS(id,"quarter"), 
                    label = "Período",
                    choices = choices_periodo #defined in R/Utils-app/filtro periodo
        )
      ),
      
      
      #Vizualisation (table or chart)
      mainPanel(
        downloadButton(NS(id,"boton")),
        br(),
        br(),
        uiOutput(NS(id,"viz"))
      )
    
      ),
    
    
    
    
  )
}

#Server ------------------------------------------------------------------------

server_sessoes_obrigatorias <- function(id,
                                               #grupo_modulo = "SGR",
                                               db_emprendedoras,
                                               db_presencas,
                                               mode = "table"
){
  
  moduleServer(id, function(input, output, session){
    
    
    grupo_modulo <- identify_grupo(id)
#parametros do modulo ======================================================
    #os parametros mudan according to the group
    parametros <- reactive({
      #funciton created in R/1.Utils-app
      #it returns all the parameters needed for this module
            parameters_grupos(grupo_modulo = grupo_modulo,
                        obrigatorias_sgr = 9,
                        obrigatorias_fnm = 15,
                        avoid = "")
      
    })
    
#data module ===============================================================
    
    
    #Create data that counts emprendedoras based on user selection
    num_emprendedoras <- reactive({
      
      count_emprendedoras(emprendedoras_db = db_emprendedoras,
                          grupo = grupo_modulo,
                          agrupar_por = "Por cidade")
    })
    
    #data for the table
    data_tabela <- reactive({
      
     db <-  presencas_de_grupo(presencas_db = db_presencas,
                         grupo = grupo_modulo,
                         avoid_actividade = parametros()$avoid #reactive
      )  %>%
        
        #count_sessoes SGR (and crete sessoes de coaching 1, 2, 3)-----------------
      # function creted in R/0.Utils-clean-data
      create_coaching()%>%
        #identify whether activity is FNM or SGR
        #count sessoes by emprendedora and by sessoes obrigatorias of this grupo
        #check whether the emprendedoras have assisted to sessoes obrigarias
        #function created in R/0.Utils-clean-data/tabela_sessoes_obrigatorias.R
        #the result is a table with the number of sessoes obrigatorias by Emprendedoras
        tabela_sessoes_obrigatorias(.,
                                    grupo_modulo,
                                    by= c("ID_BM","Emprendedora","actividade_grupo","Cidade"),
                                    obrigatorias_sgr = parametros()$obrigatorias_sgr,
                                    obrigatorias_fnm = parametros()$obrigatorias_fnm)
      
     
     #count assistencias de parceiros
      if(str_detect(grupo_modulo, "SGR")){
        
        db_sgr <- asistencias_parceiros(path = "data/1.zoho/3.clean/sgr.rds") #function in R/0.utils-clean-data
        db <- db %>% left_join(db_sgr, by = "Emprendedora")
        
      }
      
     db
      
    })
    
    
    
    
    #texto intro================================================================
    output$intro <- renderUI({
      
      if(grupo_modulo == "SGR"){
        
        obrigatorias <- glue('{parametros()$obrigatorias_sgr} sessões de SGR')
        
        
        
      } else if (grupo_modulo == "FNM"){
        
        obrigatorias <- glue('{parametros()$obrigatorias_fnm} sessões de FNM')
        
      } else if (grupo_modulo == "SGR + FNM"){
        
        obrigatorias <- glue('{parametros()$obrigatorias_sgr} sessões de SGR e {parametros()$obrigatorias_fnm} sessões de FNM')
      }
      
      texto <- glue('As empreendedoras da abordagem {parametros()$abordagem} devem participar de pelo menos {obrigatorias}.
                   A tabela baixo mostra o número de empreendedoras que cumprem o número de sessões obrigatórias.
                    ')
      
      h5(texto)
      
    })
    
    
#Download data =================================================================
    
    #download data
    
    output$boton <- downloadHandler(
      filename = function() {
        paste('sessoesSGR-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(data_tabela(), con)
      }
    )
    
    
    #tabela ========================================================================
    if(mode == "table"){
      
      output$viz <- renderUI({
        DT::renderDT({
          data_tabela()
        },
        extensions = 'Buttons',
        options = list(
          language = "pt",
          dom = 'Blfrtip'
        ),
        rownames= FALSE
        )#renderDT
        
      }) #renderUI
      
    } #mode table
    
    
  
 #chart ========================================================================
    
    if(mode == "chart"){
      
      output$viz <- renderUI({
        
        fluidRow(
        renderPlotly({
        
            data_plot <- data_tabela() %>%
              group_by(Cidade) %>%
              summarise(total = sum(cumple),.groups = 'drop')
            
            
            plot <- data_plot %>%
              #created in R/3.Plots/plot_obrigatorias.R
              plot_obrigatorias(.,num_emprendedoras())
          
          
          # if(grupo_modulo == "SGR + FNM"){
          #   
          #   data_plot <- data_tabela() %>%
          #     group_by(Cidade) %>%
          #     summarise(`Sessoes fnm` = sum(cumple_fnm),
          #               `Sessoes Sgr` = sum(cumple_sgr),
          #               .groups = 'drop') %>%
          #     pivot_longer(-Cidade,
          #                  names_to = "grupo",
          #                  values_to = "total"
          #                  )
          #   
          #   plot <- data_plot %>%
          #     ggplot(aes(x = Cidade,
          #                y = total,
          #                fill = grupo
          #                )) +
          #     geom_col() +
          #     scale_fill_manual(values = palette) +
          #     theme_realiza()
          #   
          # }
          
        
        plot
        }) #renderPlotly
        ) #fluidRow
        
      }) #renderUI
      
    }
    
  })
}


#create data for this module








