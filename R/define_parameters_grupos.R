#' define parameters for the dashboard according to the groups
#' 

#'Defines the main selector ===================================================
#'@returns name of the main dropdown (Agente for FNM and Turma for SGR)
define_selector <- function(x){
  
  if(x == "fnm"){
    
    title_selector <- "Agente"
    
  } else if(x == "sgr"){
    
    title_selector <- "Turma"
    
  } else {
    
    title_selector <- "Agente"
  }
  
  return(title_selector)
  
}



#define accronym ===============================================================


define_accronym <- function(x){
  
  if(x == "fnm"){
    
    accr <- "FNM"
      
    
    
  } else if(x == "sgr"){
    
    accr <- "SGR"
      
  
    
  } else {
    
    accr <-  "SGR + FNM"
  }
  
  return(accr)
  
}




#"Define legend of the table ===================================================
#' the bolinhas that explain the status of the activities

define_legend <- function(x){

  
  if(grepl("sessoes", x)){
    
    legend <- tags$p(
      tags$div(class = "dot green"), "Presente",
      tags$div(class = "dot red"), "Ausente",
      tags$div(class = "dot blue"), "Agendado",
      tags$div(class = "dot yellow", "X"), "Agente não marcou",
      tags$div(class = "dot empty"), "Sem Agenda"
      
    ) }
    
 else {
    
    legend <- 
    tags$p(
      tags$div(class = "dot green"), "Presente",
      tags$div(class = "dot red"), "Ausente",
      tags$div(class = "dot empty"), "Ainda não aconteceu"
      
    )
  }
  
  

  
  return(legend)
  
}
  

  
  
