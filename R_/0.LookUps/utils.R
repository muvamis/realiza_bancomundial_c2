#'Utils lookup
#'

#' get value from variables formatted as lists in Zoho ===========================

clean_zoho_lisr <- function(x){
  
  ifelse(str_detect(x,"list"),
         str_extract(x, '(?<=display_value = ").*?(?=", ID)'),
         x)
}


