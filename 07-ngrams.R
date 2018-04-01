library(readr)
library(ggplot2)
library(purrr)
library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(viridis)
library(lubridate)
library(rcorpora)

diputados <- readRDS("data/pdf_diputados")
senadores <- readRDS("data/pdf_senadores")

# me deshago de los saltos de línea que sólo los voy a usar más adelante
diputados <- diputados %>%
  mutate(pdf = stri_replace_all(pdf, replacement = "", regex = "\\\n"))

senadores <- senadores %>%
  mutate(pdf = stri_replace_all(pdf, replacement = "", regex = "\\\n"))

# ------------ ngrams ------------

diputados %>%
  tidytext::unnest_tokens(ngram, pdf, token = "ngrams", n = 3) %>% 
  filter(str_detect(ngram, "sendic")) %>% 
  count(ngram, sort = TRUE) %>%
  top_n(15) %>%
  mutate(ngram = reorder(ngram, n)) %>%
  ggplot(aes(ngram, n)) +
  geom_col(fill = "red", show.legend = FALSE) +
  xlab(NULL) +
  ylab(NULL) +
  coord_flip() +
  theme_minimal()

diputados %>%
  tidytext::unnest_tokens(ngram, pdf, token = "ngrams", n = 3) %>% 
  filter(str_detect(ngram, "ancap")) %>% 
  count(ngram, sort = TRUE) %>%
  top_n(10) %>%
  mutate(ngram = reorder(ngram, n)) %>%
  ggplot(aes(ngram, n)) +
  geom_col(fill = "red", show.legend = FALSE) +
  xlab(NULL) +
  ylab(NULL) +
  coord_flip() +
  theme_minimal()

diputados %>%
  tidytext::unnest_tokens(ngram, pdf, token = "ngrams", n = 3) %>% 
  filter(str_detect(ngram, "renuncia")) %>% 
  count(ngram, sort = TRUE) %>%
  top_n(10) %>%
  mutate(ngram = reorder(ngram, n)) %>%
  ggplot(aes(ngram, n)) +
  geom_col(fill = "red", show.legend = FALSE) +
  xlab(NULL) +
  ylab(NULL) +
  coord_flip() +
  theme_minimal()
  