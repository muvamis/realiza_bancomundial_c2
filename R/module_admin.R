#Module sessoes
library(dplyr)
library(ggplot2)
library(shinycssloaders)

#Check password ===============================================================
data_login <- tibble(
  user = c("admin", "Andres", "MEL"),
  password = c("admin", "admin", "MEL")
)

ModalAdmin <- function(failed= FALSE){
  
  modalDialog(
    title = "Login",
    #textInput("password", "Password"),
    textInput("user", "User"),
    passwordInput("password", "Password"),
    if (failed)
      div(tags$b("O usuário ou a senha digitada estão incorretos", style = "color: red;")),
    
    easyClose = FALSE,
    footer = tagList(
      actionButton("ok", "OK")
    ))
  
}




#Ui ============================================================================
ui_admin <- function(id){
 
  
   
   tagList(
     
   
     actionButton(NS(id,"btn_atualizar"), "Atualizar data"),
     actionButton(NS(id, "btn_check"), "Criar checks aleatórios"),
     
     br(),
     br(),
     #Let the user know that all is fine --------------------------------------------------------------
     #table checks
     shinycssloaders::withSpinner(DT::dataTableOutput(NS(id,"table_check")), color = "red"),
     #table updated files
     #let the user know to refresh the page when downloaded
     uiOutput(NS(id,"ui_refresh_msg")),
     shinycssloaders::withSpinner(DT::dataTableOutput(NS(id,"table_dwnlds")), color = "red"),
     #buton download checks
     br(),
     uiOutput(NS(id,"ui_dwld_checks"))
     
     
     
     
 )
  
}



#Server ======================================================================
serverAdmin <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    
  #create random check  -----------------------------------------
  
    data_checks <- eventReactive(input$btn_check,{
      
      #function saved in R
      #it reads the clean presencas and selects a record by city
      #this record is the latest present record of an emprendora
      create_random_check()
      
    })
    
    
    #display checks to user
    output$table_check <- DT::renderDataTable({
      
      data_checks()
    })
    
    #download random checks -----------------------------------------------
    output$ui_dwld_checks <- renderUI({
      
      req(data_checks())
      
      downloadButton(NS(id,"downloadChecks"), "Baixar tabela de verificação")
      
      
    })
    
    
    output$downloadChecks <- downloadHandler(
      
      
      filename = function() {
        paste("verificacao_realiza_",Sys.Date(),".csv", sep = "")
      },
      content = function(file) {
        write.csv(data_checks(), file, row.names = FALSE)
      }
    )
    
    
#Download data =================================================================
    data_dwln <- eventReactive(input$btn_atualizar, {
      
      
      source("R_/X.Run_flow.R", encoding = "UTF-8")
      
      tibble(Files = list.files("data/2.Dashboard"))
      
      
    })
    
    
#inform the user which files have been updated
    output$table_dwnlds <- DT::renderDataTable({
      
      data_dwln()
    })
    
    
    text_info <- eventReactive( data_dwln(), {
      
      "Atualização bem-sucedida, atualize a página ou pressione Ctrl + R para que o painel leia os dados atualizados"
      
    })
    
    output$ui_refresh_msg <- renderUI({
      
      h1(text_info())
    })
   
     
   
    
    
    
  })
}