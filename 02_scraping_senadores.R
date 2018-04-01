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
date_end  <- "31-03-2018"

#······················
#      senadores
#······················

url_senadores <- paste0("https://parlamento.gub.uy/documentosyleyes/documentos/diarios-de-sesion?Cpo_Codigo_2=S&Lgl_Nro=48&DS_Fecha%5Bmin%5D%5Bdate%5D=",
                         date_init,
                        "&DS_Fecha%5Bmax%5D%5Bdate%5D=",
                        date_end,
                        "&Ssn_Nro=&TS_Diario=&tipoBusqueda=T&Texto=")

# extraigo pdfs

pdf_senadores_pag1 <- extract_pdf(url_senadores, pag = 1)
pdf_senadores_pag2 <- extract_pdf(url_senadores, pag = 2)

# junto todos los pdfs
pdf_senadores <- bind_rows(pdf_senadores_pag1, pdf_senadores_pag2) %>% 
  distinct() # no hay duplicados como con los diputados, pero por las dudas


# extraigo fechas

pdf_fechas_senadores_pag1 <- extract_metadata(url_senadores, info = "fecha", pag = 1)
pdf_fechas_senadores_pag2 <- extract_metadata(url_senadores, info = "fecha", pag = 2)

# junto todos las fechas y las convierto en un df
pdf_fechas_senadores <- c(pdf_fechas_senadores_pag1, pdf_fechas_senadores_pag2) %>% 
  tbl_df() %>% 
  transmute(fecha = as.Date(value, "%d-%m-%Y"))

# extraigo sesiones

pdf_sesion_senadores_pag1 <- extract_metadata(url_senadores, info = "sesion", pag = 1)
pdf_sesion_senadores_pag2 <- extract_metadata(url_senadores, info = "sesion", pag = 2)

# junto todos las sesiones y las convierto en un df
pdf_sesion_senadores <- c(pdf_sesion_senadores_pag1, pdf_sesion_senadores_pag2) %>% 
  tbl_df() %>% 
  rename(sesion = value)

# hago un df con todo
pdf_senadores <- bind_cols(pdf_fechas_senadores, pdf_sesion_senadores, pdf_senadores) %>% 
  unite("fecha_sesion", c(fecha, sesion), remove = FALSE) %>% 
  distinct()  # no hay duplicados como con los diputados, pero por las dudas


# saveRDS(pdf_senadores, "data/pdf_senadores")
