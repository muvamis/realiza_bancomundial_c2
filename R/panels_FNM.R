

panels_FNM <- function(id){
  

    tabPanel(id,
             value = "cidades_fnm",
             ui_cidades("fnm_cidades") 
             )
    # navbarMenu(id,
    #            tabPanel("Por Cidade",
    #                     value = "cidades_fnm",
    #                     ui_cidades("fnm_cidades")   
    #            )
    # )
    
  
}


