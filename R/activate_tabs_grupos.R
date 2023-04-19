#'Activate the server of the groups when their tab is selected



activate_tabs_grupos <- function(input, session, grupos, tipos){
  
  #walk each grupo
  lapply(grupos, function(grupo){
    
    #Create ids for tabs and servers (defined in the panels of each group)
    lapply(tipos, function(tipo){
      
      id_tab <- paste0(tipo,"_", grupo)
      id_server <- paste0(grupo,"_",tipo)
      
      #Activate the server of each tab when it is selected
      observe({
        
        if(input$Paneles == id_tab){
          
          #run server cidades
          if(tipo == "cidades"){
            
            serverCidade(id_server, grupo = grupo)
            
            #run server sessoes
          } else if(tipo == "agenda"){
            
            
            serverAgenda(id_server, grupo)
            
            
            }else {
            
            serverSessoes(id_server, grupo = grupo, tipo)
            
          }
          
        }
        #message(input$Paneles)
        
      })
      
      
      
    })
    
    
  })
  
}
