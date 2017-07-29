library(rvest)
library(purrr)
library(pdftools)

#······················
#     setup dates
#······················

date_init <- "01-01-2017"
date_end  <- "14-02-2020"

#······················
#      diputados
#······················

url_diputados <- paste0("https://parlamento.gub.uy/documentosyleyes/documentos/diarios-de-sesion?Cpo_Codigo_2=D&Lgl_Nro=48&DS_Fecha%5Bmin%5D%5Bdate%5D=",
                        date_init,
                        "&DS_Fecha%5Bmax%5D%5Bdate%5D=",
                        date_end,
                        "&Ssn_Nro=&TS_Diario=&tipoBusqueda=T&Texto=")

pdf_diputados <- url_diputados %>% 
  read_html() %>% 
  html_nodes(".views-field-DS-File-IMG a") %>%   # seleccionar clase
  html_attr("href") %>% 
  map( ~ paste0("https://parlamento.gub.uy", .)) %>% 
  unlist() %>% 
  map( ~ pdf_text(.))

#······················
#      senadores
#······················

url_senadores <- paste0("https://parlamento.gub.uy/documentosyleyes/documentos/diarios-de-sesion?Cpo_Codigo_2=S&Lgl_Nro=48&DS_Fecha%5Bmin%5D%5Bdate%5D=",
                        date_init,
                        "&DS_Fecha%5Bmax%5D%5Bdate%5D=",
                        date_end,
                        "&Ssn_Nro=&TS_Diario=&tipoBusqueda=T&Texto=")

pdf_senadores <- url_senadores %>% 
  read_html() %>% 
  html_nodes(".views-field-DS-File-IMG a") %>%   # seleccionar clase
  html_attr("href") %>% 
  map( ~ paste0("https://parlamento.gub.uy", .)) %>% 
  unlist() %>% 
  map( ~ pdf_text(.))

library(stringr)

pdf_fechas_senadores <- url_senadores %>% 
  read_html() %>% 
  html_nodes("td.views-field-DS-Fecha") %>% 
  html_text() 

pdf_fechas_senadores %>% 
  map(str_extract(., "^\n .([.ˆ ]) ."))



save.image("data.Rdata")
