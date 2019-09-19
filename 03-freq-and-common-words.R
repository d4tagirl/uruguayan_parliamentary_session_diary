diputados <- readRDS("data/pdf_diputados")
senadores <- readRDS("data/pdf_senadores")

library(purrr)
library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(viridis)
library(lubridate)
library(rcorpora)
library(scales)
library(zoo)
library(gridExtra)


# me deshago de los saltos de línea que sólo los voy a usar más adelante
diputados <- diputados %>%
  mutate(pdf = stri_replace_all(pdf, replacement = "", regex = "\\\n"))

senadores <- senadores %>%
  mutate(pdf = stri_replace_all(pdf, replacement = "", regex = "\\\n"))

# ------------ frecuencia de sesiones ---------------

freq_diputados <- ggplot(diputados, aes(as.factor(as.yearmon(fecha)), fill=as.factor(as.yearmon(fecha)))) + 
  geom_bar(position='dodge', show.legend = FALSE) + 
  ylab("Cantidad de sesiones") + 
  xlab("") + 
  scale_x_discrete(limits = as.factor(as.yearmon(seq.Date(floor_date(min(senadores$fecha), "month"), floor_date(max(senadores$fecha), "month"), "month")))) +
  theme_minimal() +
  theme(axis.text.x=element_blank()) +
  ggtitle("Sesiones de Diputados")

freq_senadores <- ggplot(senadores, aes(as.factor(as.yearmon(fecha)), fill=as.factor(as.yearmon(fecha)))) + 
  geom_bar(position='dodge', show.legend = FALSE) +
  xlab("") + 
  ylab("Cantidad de sesiones") +
  scale_x_discrete(limits = as.factor(as.yearmon(seq.Date(floor_date(min(senadores$fecha), "month"), floor_date(max(senadores$fecha), "month"), "month")))) +
  theme_minimal() +
  theme(axis.text.x  = element_text(angle=45,
                                    hjust = 1,
                                    vjust = 1)) +
  ggtitle("Sesiones de Senadores")

grid.arrange(freq_diputados, freq_senadores)


# -------- largo de las sesiones por mes, medido en cantidad de palabras -----------


# Diputados

session_diputados_words_mes <- diputados %>%
  unnest_tokens(word, pdf) %>%
  mutate(mes = as.yearmon(fecha)) %>% 
  count(mes, sort = TRUE) %>%
  ungroup()

cant_sesiones_diputados_mes <- diputados %>% 
  group_by(mes = as.yearmon(fecha)) %>% 
  summarise(cant_sesiones = n()) %>%
  ungroup()

session_diputados_words_mes <- left_join(session_diputados_words_mes, cant_sesiones_diputados_mes) %>% 
  mutate(palabras_prom_sesion = n/cant_sesiones)


# Senadores

session_senadores_words_mes <- senadores %>%
  unnest_tokens(word, pdf) %>%
  mutate(mes = as.yearmon(fecha)) %>% 
  count(mes, sort = TRUE) %>%
  ungroup()

cant_sesiones_senadores_mes <- senadores %>% 
  group_by(mes = as.yearmon(fecha)) %>% 
  summarise(cant_sesiones = n()) %>%
  ungroup()

session_senadores_words_mes <- left_join(session_senadores_words_mes, cant_sesiones_senadores_mes) %>% 
  mutate(palabras_prom_sesion = n/cant_sesiones)

# Visualizo

prom_palabras_sesiones_diputados <- ggplot(session_diputados_words_mes, aes(x = as.factor(mes), fill=as.factor(mes))) + 
  geom_col(aes(y = palabras_prom_sesion), show.legend = FALSE) + 
  scale_y_continuous(limit=c(0,150000)) +
  ylab("Cantidad de palabras promedio \n de las sesiones del mes") + 
  xlab("") + 
  scale_x_discrete(limits = as.factor(as.yearmon(seq.Date(floor_date(min(senadores$fecha), "month"), floor_date(max(senadores$fecha), "month"), "month")))) +
  theme_minimal() +
  theme(axis.text.x=element_blank()) +
  ggtitle("Diputados")

prom_palabras_sesiones_senadores <- ggplot(session_senadores_words_mes, aes(x = as.factor(mes), fill=as.factor(mes))) + 
  geom_col(aes(y = palabras_prom_sesion), show.legend = FALSE) + 
  scale_y_continuous(limit=c(0,150000)) + 
  scale_x_discrete(limits = as.factor(as.yearmon(seq.Date(floor_date(min(senadores$fecha), "month"), floor_date(max(senadores$fecha), "month"), "month")))) +
  xlab("") + 
  ylab("Cantidad de palabras promedio \n de las sesiones del mes") +
  theme_minimal() +
  theme(axis.text.x  = element_text(angle=45,
                                    hjust = 1,
                                    vjust = 1)) +
  ggtitle("Senadores")

grid.arrange(prom_palabras_sesiones_diputados, prom_palabras_sesiones_senadores)

# busco las sesiones más extremas: 
# Diputados:
#   Feb 2017: violencia en el fútbol
# 
# diputados %>%
#   mutate(mes = as.yearmon(fecha)) %>% 
#   filter(mes == "Feb 2017") %>% 
#   unnest_tokens(word, pdf) %>% 
#   count(sesion, sort = TRUE) %>%
#   ungroup()
# 
# #   Ago 2018: 
# 
# diputados %>%
#   mutate(mes = as.yearmon(fecha)) %>% 
#   filter(mes == "Aug 2018") %>% 
#   unnest_tokens(word, pdf) %>% 
#   count(sesion, sort = TRUE) %>%
#   ungroup()
# 
# 3 días de debate en sesiones especiales de RENDICIÓN DE CUENTAS Y BALANCE DE EJECUCIÓN PRESUPUESTAL
# Dic 2017
# diputados %>%
#   mutate(mes = as.yearmon(fecha)) %>%
#   filter(mes == "Dec 2017") %>%
#   unnest_tokens(word, pdf) %>%
#   count(sesion, sort = TRUE) %>%
#   ungroup()

# senadores:
# Setiembre 2017 y 2018:
#   Rend de cuentas
# senadores %>%
#   mutate(mes = as.yearmon(fecha)) %>%
#   filter(mes == "Apr 2018") %>%
#   unnest_tokens(word, pdf) %>%
#   count(sesion, sort = TRUE) %>%
#   ungroup()
# Nov 2017: Senado debatió acuerdo entre UPM y gobierno

#Abril 2018: La octava interpelación a Bonomi será el 18 de abril


# --------- most common words ------

stopwords <- corpora("words/stopwords/es")$stopWords

stopwords <- c(stopwords, as.character(seq(1:50)))

# Diputados

tidy_diputados <- diputados %>%
  tidytext::unnest_tokens(word, pdf) %>% 
  filter(!word %in% stopwords) 

most_used_dip <- tidy_diputados %>%
  count(word, sort = TRUE) %>%
  ungroup()

most_used_dip %>%
  top_n(30) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = word)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  theme_minimal() +
  scale_fill_viridis(discrete = TRUE)


# Senadores

tidy_senadores <- senadores %>%
  tidytext::unnest_tokens(word, pdf) %>% 
  filter(!word %in% stopwords) 

most_used_sen <- tidy_senadores %>%
  count(word, sort = TRUE) %>%
  ungroup()

most_used_sen %>%
  top_n(30) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = word)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  theme_minimal() +
  scale_fill_viridis(discrete = TRUE)

