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

# ---------------- stopwords ----------------------

stopwords <- corpora("words/stopwords/es")$stopWords

custom_stopwords <- c("senor", "presidente", "representante",
                      "representantes", "dia", "mas", "ley", "nacional",
                      "camara", "senora", "consideracion", "presente",
                      "republica", "articulo", "saluda", "atentamente", "nº",
                      "motivos", "tambien", "2017", "c", "1º", "motivo", "n")

stopwords <- c(stopwords, custom_stopwords)

numeros <- as.character(seq(1:50))

nombres <- c("carlos", 
             "jose", 
             "mahia")

meses <- c("enero", 
           "febrero", 
           "marzo", 
           "abril", 
           "mayo", 
           "junio", 
           "julio", 
           "agosto", 
           "setiembre", 
           "octubre", 
           "noviembre", 
           "diciembre")

departamentos <- c('artigas',
                   'canelones',
                   'cerro largo',
                   'colonia',
                   'durazno',
                   'flores',
                   'florida',
                   'lavalleja',
                   'maldonado',
                   'montevideo',
                   'paysandu',
                   'rio negro',
                   'rivera',
                   'rocha',
                   'salto',
                   'san jose',
                   'soriano',
                   'tacuarembo',
                   'treinta y tres')

# (http://www.ine.gub.uy/documents/10181/35289/analisispais.pdf/cc0282ef-2011-4ed8-a3ff-32372d31e690)
poblacion_departamental <- tibble(departamento = departamentos, poblacion = c(
  73378,
  520187,
  84698,
  123203,
  57088,
  25050,
  67048,
  58815,
  164300,
  1319108,
  113124,
  54765,
  103493,
  68088,
  124878,
  108309,
  82595,
  90053,
  48134))


# ------------------ tidytext: counting mentions to departamentos --------------

tidy_diputados_departamentos <- diputados %>%
  str_detect(pdf, departamentos)
  tidytext::unnest_tokens(word, pdf) %>% 
  filter(word %in% departamentos) 

tidy_diputados_departamentos %>%
  count(word, sort = TRUE) %>%
  ungroup() %>%
  inner_join(poblacion_departamental, by = c("word" = "departamento")) %>% 
  ggplot(aes(x = reorder(word, n))) +
  geom_col(aes(y = n, fill = reorder(word, n), alpha = 1/10), show.legend = FALSE) +
  geom_point(aes(y = (poblacion)/min(n)), position = "dodge", show.legend = FALSE) +
  # facet_wrap(~fecha_sesion, scales = "free_x") +
  coord_flip() +
  theme_minimal() +
  scale_fill_viridis(discrete = TRUE)



# palabras mas usadas en distintas sesiones... ver qué encuentro, quizás hacerlo para las más negativas y más positivas???


# ------------------ tidytext: counting words --------------

stopwords_con_numeros <- c(stopwords, numeros, nombres)

tidy_diputados <- diputados %>%
  tidytext::unnest_tokens(word, pdf) %>% 
  filter(!word %in% stopwords_con_numeros) 

# --------- most common words ------

most_used <- tidy_diputados %>%
  count(word, sort = TRUE) %>%
  ungroup()

most_used %>%
  top_n(50) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = word)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  theme_minimal() +
  scale_fill_viridis(discrete = TRUE)

# ---------- 

most_used_topics <- tidy_diputados %>%
  filter(word %in% c(
    
    "educacion",
    
    "seguridad",
    "policia",
    "futbol",
    "violencia",
    "interior",
    "sociedad",
    "deporte",
    "admision",
    
    "ancap",
    "terrorismo",
    "justicia",
    "canelones",
    
    "mujeres",
    
    "transporte",
    "exterior",
    "penal",
    "dictadura",
    "negociacion",
    
    
    "cultura",
    
    "drogas",
    "marihuana",
    "farmacia",
    "farmacias",
    "19.172",
    "cannabis",
    "ircca",
    
    "gestion",
    "banca",
    "economia",
    "recursos",
    "rendicion",
    "venezuela",
    "humanos",
    
    "banco",
    "canelones",
    
    "17.827",
    "campo"
  )) %>%  
  group_by(fecha_sesion) %>%
  count(word, sort = TRUE) %>% 
  ungroup

most_used_topics <- most_used_topics %>% 
  ungroup %>% 
  expand(fecha_sesion, word) %>% 
  left_join(most_used_topics, by = c("fecha_sesion" = "fecha_sesion", "word" = "word")) %>% 
  complete(nesting(fecha_sesion, word), fill = list(n = 0))


most_used_topics %>%
  # top_n(5) %>%
  # ungroup() %>%
  # mutate(word = reorder(word, n)) %>%
  ggplot(aes(month(fecha_sesion), n, fill = word)) +
  geom_col(position = "dodge") +
  # facet_grid(~ date, scales = "free") +
  # facet_wrap(~date + session, scales = "free") +
  # coord_flip() +
  theme_minimal() # +
# scale_fill_viridis(discrete = TRUE)




# ordinarias

most_used_by_ordinary <- tidy_diputados %>%
  filter(session == "ordinaria",
         !word %in% c(meses, "montevideo", "licencia", "comision")) %>% 
  group_by(date) %>% 
  count(word, sort = TRUE) 

most_used_by_ordinary %>%
  top_n(10) %>%
  ggplot(aes(reorder(word, n), n, fill = as.factor(date))) +
  geom_col(show.legend = FALSE) +
  facet_wrap( ~ date, scales = "free") +
  coord_flip() +
  theme_minimal() +
  scale_fill_viridis(discrete = TRUE)

# extraordinarias

most_used_by_extraordinary <- tidy_diputados %>%
  filter(session == "extraordinaria",
         !word %in% c(meses, "montevideo", "licencia", "comision")) %>% 
  group_by(date) %>% 
  count(word, sort = TRUE) 

most_used_by_extraordinary %>%
  top_n(10) %>%
  ggplot(aes(reorder(word, n), n, fill = as.factor(date))) +
  geom_col(show.legend = FALSE) +
  facet_wrap( ~ date, scales = "free") +
  coord_flip() +
  theme_minimal() +
  scale_fill_viridis(discrete = TRUE)

# solemne

most_used_by_solemne <- tidy_diputados %>%
  filter(session == "solemne",
         !word %in% c(meses, "montevideo", "licencia", "comision")) %>% 
  group_by(date) %>% 
  count(word, sort = TRUE) 

most_used_by_solemne %>%
  top_n(10) %>%
  ggplot(aes(reorder(word, n), n, fill = as.factor(date))) +
  geom_col(show.legend = FALSE) +
  facet_wrap( ~ date, scales = "free") +
  coord_flip() +
  theme_minimal() +
  scale_fill_viridis(discrete = TRUE)

# especial

most_used_by_especial <- tidy_diputados %>%
  filter(session == "especial",
         !word %in% c(meses, "montevideo", "licencia", "comision")) %>% 
  group_by(date) %>% 
  count(word, sort = TRUE) %>% 
  top_n(10, n) %>%
  mutate(order = row_number()) %>% 
  top_n(10, -order) %>%
  ungroup()

most_used_by_especial %>%
  ggplot(aes(order, n, fill = as.factor(date))) +
  geom_col(show.legend = FALSE) +
  facet_wrap( ~ date, scales = "free") +
  coord_flip() +
  theme_minimal() +
  scale_x_continuous(
    breaks = most_used_by_especial$order,
    labels = most_used_by_especial$word,
    expand = c(0,0)
  ) +
  scale_fill_viridis(discrete = TRUE)

# 
# library(wordcloud)
# library(viridis)
# library(tm)
# library(stringr)
# library(lubridate)
# 
# words <- toString(tidy_diputados$word) %>%
#   str_split(pattern = " ", simplify = TRUE)
# 
# wordcloud(words, colors = viridis::viridis_pal(end = 0.8)(10),
#           min.freq = 200, random.color = TRUE, max.words = 100,
#           scale = c(3.5,.03))

save.image("data.Rdata")
