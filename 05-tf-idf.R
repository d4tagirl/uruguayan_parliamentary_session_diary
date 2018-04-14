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
library(stringi)

diputados <- readRDS("data/pdf_diputados")
senadores <- readRDS("data/pdf_senadores")

# me deshago de los saltos de línea que sólo los voy a usar más adelante
diputados <- diputados %>%
  mutate(pdf = stri_replace_all(pdf, replacement = "", regex = "\\\n"))

senadores <- senadores %>%
  mutate(pdf = stri_replace_all(pdf, replacement = "", regex = "\\\n"))

# --------------- tf-idf por sesion -----------------

# Diputados

session_diputados_words <- diputados %>%
  unnest_tokens(word, pdf) %>%
  count(fecha, sesion, fecha_sesion, word, sort = TRUE) %>%
  ungroup()

diputados_words <- session_diputados_words %>% 
  group_by(fecha_sesion) %>% 
  summarize(total = sum(n))

session_diputados_words <- left_join(session_diputados_words, diputados_words)

# ggplot(session_diputados_words, aes(n/total, fill = fecha_sesion)) +
#   geom_histogram(show.legend = FALSE) +
#   xlim(NA, 0.0009) +
#   facet_wrap(~fecha_sesion, scales = "free_y")

session_diputados_words <- session_diputados_words %>%
  bind_tf_idf(word, fecha_sesion, n)

session_diputados_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

# Como son muchas sesiones para mirarlas juntas, las miro por períodos 

session_diputados_words %>%
  filter(fecha < as.Date("2017-05-01")) %>% 
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(fecha_sesion) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = fecha_sesion)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~fecha_sesion, ncol = 3,  scales = "free") +
  coord_flip()

session_diputados_words %>%
  filter(fecha < as.Date("2017-07-01"),
         fecha > as.Date("2017-05-01")) %>% 
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(fecha_sesion) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = fecha_sesion)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~fecha_sesion, ncol = 3,  scales = "free") +
  coord_flip()

# así se podría hacer hasta cubrir todo el período


# Senadores

session_senadores_words <- senadores %>%
  unnest_tokens(word, pdf) %>%
  count(fecha, sesion, fecha_sesion, word, sort = TRUE) %>%
  ungroup()

senadores_words <- session_senadores_words %>% 
  group_by(fecha_sesion) %>% 
  summarize(total = sum(n))

session_senadores_words <- left_join(session_senadores_words, senadores_words)

# ggplot(session_senadores_words, aes(n/total, fill = fecha_sesion)) +
#   geom_histogram(show.legend = FALSE) +
#   xlim(NA, 0.0009) +
#   facet_wrap(~fecha_sesion, scales = "free_y")

session_senadores_words <- session_senadores_words %>%
  bind_tf_idf(word, fecha_sesion, n)

session_senadores_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

# por períodos, como hice con diputados
session_senadores_words %>%
  filter(fecha < as.Date("2017-05-01")) %>% 
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(fecha_sesion) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = fecha_sesion)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~fecha_sesion, ncol = 3,  scales = "free") +
  coord_flip()

session_senadores_words %>%
  filter(fecha < as.Date("2017-07-01"),
         fecha > as.Date("2017-05-01")) %>% 
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(fecha_sesion) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = fecha_sesion)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~fecha_sesion, ncol = 3,  scales = "free") +
  coord_flip()






# --------------- tf-idf por mes -----------------

# Diputados

diputados_mes <- diputados %>% 
  mutate(mes = as.factor(as.yearmon(fecha)))

session_diputados_words_mes <- diputados_mes %>%
  unnest_tokens(word, pdf) %>%
  count(mes, word, sort = TRUE) %>%
  ungroup()

diputados_words_mes <- session_diputados_words_mes %>% 
  group_by(mes) %>% 
  summarize(total = sum(n))

session_diputados_words_mes <- left_join(session_diputados_words_mes, diputados_words_mes)

ggplot(session_diputados_words_mes, aes(n/total, fill = mes)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~mes, scales = "free_y")

session_diputados_words_mes <- session_diputados_words_mes %>%
  bind_tf_idf(word, mes, n)

session_diputados_words_mes %>%
  select(-total) %>%
  arrange(desc(tf_idf))

tfidf_mes_diputados <- session_diputados_words_mes %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(mes) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = mes)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~mes, ncol = 1,  scales = "free") +
  coord_flip() +
  ggtitle("tf-idf Sesiones de Diputados")

tfidf_mes_diputados

# Diputados

senadores_mes <- senadores %>% 
  mutate(mes = as.factor(as.yearmon(fecha)))

session_senadores_words_mes <- senadores_mes %>%
  unnest_tokens(word, pdf) %>%
  count(mes, word, sort = TRUE) %>%
  ungroup()

senadores_words_mes <- session_senadores_words_mes %>% 
  group_by(mes) %>% 
  summarize(total = sum(n))

session_senadores_words_mes <- left_join(session_senadores_words_mes, senadores_words_mes)

ggplot(session_senadores_words_mes, aes(n/total, fill = mes)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~mes, scales = "free_y")

session_senadores_words_mes <- session_senadores_words_mes %>%
  bind_tf_idf(word, mes, n)

session_senadores_words_mes %>%
  select(-total) %>%
  arrange(desc(tf_idf))

tfidf_mes_senadores <- session_senadores_words_mes %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(mes) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = mes)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~mes, ncol = 1,  scales = "free") +
  coord_flip() +
  ggtitle("tf-idf Sesiones de senadores")

tfidf_mes_senadores

grid.arrange(tfidf_mes_diputados, tfidf_mes_senadores, ncol = 2)

