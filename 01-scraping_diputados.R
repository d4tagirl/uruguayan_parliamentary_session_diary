# packrat::restore()
packrat::init()

library(rvest)
library(purrr)
library(pdftools)
library(stringr)
library(tidyr)
library(tibble)
library(dplyr)
library(stringi)

source("functions.R")

robotstxt::get_robotstxt("https://parlamento.gub.uy")
robotstxt::paths_allowed("https://parlamento.gub.uy/documentosyleyes/documentos/diarios-de-sesion")

#······················
#     setup dates
#······················

date_init <- "01-01-2017"
date_end  <- "31-07-2019"

#······················
#      diputados
#······················

url_diputados <- paste0("https://parlamento.gub.uy/documentosyleyes/documentos/diarios-de-sesion?Cpo_Codigo_2=D&Lgl_Nro=48&DS_Fecha%5Bmin%5D%5Bdate%5D=",
                        date_init,
                        "&DS_Fecha%5Bmax%5D%5Bdate%5D=",
                        date_end,
                        "&Ssn_Nro=&TS_Diario=&tipoBusqueda=T&Texto=")

# extraigo pdfs

pdf_diputados_pag1 <- extract_pdf(url_diputados, pag = 1)
pdf_diputados_pag2 <- extract_pdf(url_diputados, pag = 2)
pdf_diputados_pag3 <- extract_pdf(url_diputados, pag = 3)
pdf_diputados_pag4 <- extract_pdf(url_diputados, pag = 4)
pdf_diputados_pag5 <- extract_pdf(url_diputados, pag = 5)


# junto todos los pdfs
pdf_diputados <- bind_rows(pdf_diputados_pag1, pdf_diputados_pag2,
                           pdf_diputados_pag3, pdf_diputados_pag4,
                           pdf_diputados_pag5)

# extraigo fechas

pdf_fechas_diputados_pag1 <- extract_metadata(url_diputados, info = "fecha", pag = 1)
pdf_fechas_diputados_pag2 <- extract_metadata(url_diputados, info = "fecha", pag = 2)
pdf_fechas_diputados_pag3 <- extract_metadata(url_diputados, info = "fecha", pag = 3)
pdf_fechas_diputados_pag4 <- extract_metadata(url_diputados, info = "fecha", pag = 4)
pdf_fechas_diputados_pag5 <- extract_metadata(url_diputados, info = "fecha", pag = 5)

# junto todos las fechas y las convierto en un df
pdf_fechas_diputados <- c(pdf_fechas_diputados_pag1, 
                          pdf_fechas_diputados_pag2,
                          pdf_fechas_diputados_pag3, 
                          pdf_fechas_diputados_pag4,
                          pdf_fechas_diputados_pag5) %>% 
  tbl_df() %>% 
  transmute(fecha = as.Date(value, "%d-%m-%Y"))

# extraigo sesiones

pdf_sesion_diputados_pag1 <- extract_metadata(url_diputados, info = "sesion", pag = 1)
pdf_sesion_diputados_pag2 <- extract_metadata(url_diputados, info = "sesion", pag = 2)
pdf_sesion_diputados_pag3 <- extract_metadata(url_diputados, info = "sesion", pag = 3)
pdf_sesion_diputados_pag4 <- extract_metadata(url_diputados, info = "sesion", pag = 4)
pdf_sesion_diputados_pag5 <- extract_metadata(url_diputados, info = "sesion", pag = 5)

# junto todos las sesiones y las convierto en un df
pdf_sesion_diputados <- c(pdf_sesion_diputados_pag1, 
                          pdf_sesion_diputados_pag2,
                          pdf_sesion_diputados_pag3, 
                          pdf_sesion_diputados_pag4,
                          pdf_sesion_diputados_pag5) %>% 
  tbl_df() %>% 
  rename(sesion = value)

# hago un df con todo
pdf_diputados <- bind_cols(pdf_fechas_diputados, pdf_sesion_diputados, pdf_diputados) %>% 
  unite("fecha_sesion", c(fecha, sesion), remove = FALSE) %>% 
  distinct() # la primer sesión de la segunda página es igual a la última sesión de la primera página
  

# saveRDS(pdf_diputados, "data/pdf_diputados")

