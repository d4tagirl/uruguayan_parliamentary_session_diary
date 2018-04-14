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

# A pesar de tener el problema mencionado en el artículo:
#La función pdftools::read_pdf() lee los renglones de izquierda a derecha. En los Diarios de Sesiones hay algunas páginas que se organizan con texto en dos columnas, entonces hay renglones que, leídos de esa forma, quedan incoherentes. Esto hay que tenerlo en cuenta para ver si el tipo de análisis que quiero hacer tiene sentido o no. Por ejemplo, si lo que quiero es analizar n-gramas donde el orden de las palabras es importante, voy a tener problemas porque estaría considerando palabras de distintas columnas de texto, como si vinieran una a continuación de la otra. 
# Voy a intentarlo de todas formas, asumiendo que si hay palabras que vienen una a continuación de la otra pero no están relacionadas porque pertenecen a diferentes columnas, la frecuencia va a ser muy baja y no me va a afectar demasiado cuando mire el top_n de los ngrams más usados.

diputados %>%
  tidytext::unnest_tokens(ngram, pdf, token = "ngrams", n = 3) %>% 
  filter(str_detect(ngram, "sendic")) %>% 
  count(ngram, sort = TRUE) %>%
  top_n(10) %>%
  mutate(ngram = reorder(ngram, n)) %>%
  ggplot(aes(ngram, n)) +
  geom_col(fill = "red", show.legend = FALSE) +
  xlab(NULL) +
  ylab(NULL) +
  coord_flip() +
  theme_minimal()

# miro trigramas que tienen la palabra ANCAP
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

# miro trigramas que tienen la palabra renuncia
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
  