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
      #filtros
      sidebarPanel(
       
        width = 2,
        
        #filtro para mostrar so para Movimenta
        uiOutput(NS(id,"ui_movimenta")),
        
        #filtro por periodo
        selectInput(NS(id,"quarter"), 
                    label = "Período",
                    #the choices for periodo are defined in 1.Utils-app/filtro_periodo.R
                    choices = choices_periodo #defined in R/Utils-app/filtro periodo
        )
      ),
      
      
      #Vizualisation (tabela o grafico)
      # Tabela is displayed in TABELA SESSOES OBLIGATORIAS
      # Grafico is displayed in SESSOES OBRIGATORIAS
      mainPanel(
        downloadButton(NS(id,"boton"), label = "Baixar Dados"),
        br(),
        br(),
        #this is the table or the grafico, depends on which page you are in
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
    
    #to work the id should contain any of "cresca", "movimenta", or "conecta"
    #identify_grupo() is defined in 1.Utils-app/identify-grupo.R
    #it detects the name of the grupo in the id of the module and returns a character
    #with the name as it is in the data (FNM or SGR or FNM + SGR)
    grupo_modulo <- identify_grupo(id) #function defined in 1.Utils-app
    
    
    
#ui Movimenta ==================================================================
    #Movimenta needs a filter to show ALL or by abordagem
    # For the others is always all
    
    filtro_movimenta_chart <- grupo_modulo == "SGR + FNM" & mode == "chart"
    
    output$ui_movimenta <- renderUI({
      
      #Only enable if grupo is Movimenta and mode is chart
      #filtro defined above
      if(filtro_movimenta_chart)
        
        selectInput(NS(id,"Componente_movimenta"), 
                    label = "Por componente",
                    choices = c("All", "Por Componente") #defined in R/Utils-app/filtro periodo
        )
      
    })

    
    #Define a reactive filtro because this filtro is only used for Movimenta
    # so the filtro has to be created artificially for the others abordagems
    # It is used to create a wrapped grafico
    
    by_componente <- reactive({
      
      #Only if grupo is Movimenta and mode is chart
      #filtro defined above
      if(filtro_movimenta_chart) {
        
        input$Componente_movimenta
        
      } else {
        
        "All"
      }
      
    })
    
#parametros do modulo ======================================================
    #os parametros mudan according to the group
    parametros <- reactive({
      #funciton created in R/1.Utils-app
      #it returns all the parameters needed for this module:
      #' abordagem 
      #' avoid: activities to avoid in the data
      #' obrigatorias_sgr: sessoes obrigatorias de sgr
      #' obrigatorias_fnm: sessoes obrigatorias de fnm
      #' obrigatorias: numero de sessoes obrigatorias
      
            parameters_grupos(grupo_modulo = grupo_modulo,
                        obrigatorias_sgr = 9,
                        obrigatorias_fnm = 15,
                        avoid = "")
      
    })
    
#data module ===============================================================
    
    
    #Create data that counts emprendedoras based on user selection
    num_emprendedoras <- reactive({
      
      
      #this function counts the number of emprendedoras registered in the programme
      #See details in 0.utils-cleandata/count_emprendedoras.R
      count_emprendedoras(emprendedoras_db = db_emprendedoras,
                          grupo = grupo_modulo,
                          agrupar_por = "Por cidade")
    })
    
    #data for the table
    data_tabela <- reactive({
      #presencas_de_grupo() is created un 0.utils-clean-data/presencas_de_grupo.R
      #it keeps the data for the given grupo, removes certain actividades that are 
      #not of interest for this analysis and keeps the given status.
     
     db <-  presencas_de_grupo(presencas_db = db_presencas,
                         grupo = grupo_modulo,
                         avoid_actividade = parametros()$avoid #reactive and created in parametros,
                         #keep = c("Presente") #to count number of sessoes agendadas
      )  %>%
        
        #count_sessoes SGR (and crete sessoes de coaching 1, 2, 3)-----------------
      # function creted in R/0.Utils-clean-data
      create_coaching()%>%
        #identify whether activity is FNM or SGR
        #count sessoes by emprendedora and by sessoes obrigatorias of this grupo
        #check whether the emprendedoras have assisted to sessoes obrigarias
        #function created in R/0.Utils-clean-data/tabela_sessoes_obrigatorias.R
        #the result is a table with the number of sessoes obrigatorias by Emprendedoras
       # actividade_grupo is a variable that identifies whether the activity is from sgr or from FNM
        tabela_sessoes_obrigatorias(.,
                                    grupo_modulo,
                                    by= c("ID_BM","Emprendedora","actividade_grupo","Cidade"),
                                    obrigatorias_sgr = parametros()$obrigatorias_sgr,
                                    obrigatorias_fnm = parametros()$obrigatorias_fnm)
      
     
     #count assistencias de parceiros
      if(str_detect(grupo_modulo, "SGR")){
        
        #the function asistencias parceiros takes the path and returns a data
        #frame with the number of times that a parceiro attended by emprendedora
        db_sgr <- asistencias_parceiros(db_presencas, grupo_modulo) #function in R/0.utils-clean-data
        db <- db %>% left_join(db_sgr, by = "Emprendedora")
        
      }
    
     print(names(db))
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
                   <br><br>
                   <b>As bolinhas mostram o número de emprendedoras registradas.E as
                   barras ou número que atendem às sessões obrigatórias. </b>
                    ')
      
      h5(HTML(texto))
      
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
      #
      #print(input$by)
      
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
        
          #Filter because Movimenta needs to have both (ALL and Por Componente)
          if(by_componente() == "All" ) {
            data_plot <- data_tabela() %>%
              group_by(Cidade) %>%
              summarise(total = sum(cumple),.groups = 'drop')
            
            
            plot <- data_plot %>%
              #created in R/3.Plots/plot_obrigatorias.R
              plot_obrigatorias(.,num_emprendedoras())
          }
          
          #This is only for Movimenta 
            if(by_componente() == "Por Componente" ){

            data_plot <- data_tabela() %>%
              group_by(Cidade) %>%
              summarise(FNM = sum(cumple_fnm),
                        SGR = sum(cumple_sgr),
                        .groups = 'drop') %>%
              pivot_longer(-Cidade,
                           names_to = "grupo",
                           values_to = "total"
                           )

            plot <- data_plot %>%
              ggplot(aes(x = Cidade,
                         y = total,
                          fill = Cidade
                         )) +
              geom_col() +
              labs(x = "")+
              scale_fill_manual(values = palette) +
              facet_wrap( ~ grupo) +
              geom_point(data = num_emprendedoras()) +
              theme_realiza()

          }
          
        #plot as plotly 
          
          ggplotly(plot) %>%
            config(displayModeBar = F) #avoid selections on the top
        }) #renderPlotly
        ) #fluidRow
        
      }) #renderUI
      
    }
    
  })
}


#create data for this module








