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


# ------------------ con http://web.eecs.umich.edu/~mihalcea/downloads/SpanishSentimentLexicons.tar.gz


lexicon <- read_tsv("SpanishSentimentLexicons/fullStrengthLexicon.txt",
                    col_names = FALSE) %>% 
  select(X1, X3) %>% 
  rename(palabra = X1, sentimiento = X3)

stopwords <- corpora("words/stopwords/es")$stopWords

stopwords <- c(stopwords, as.character(seq(1:50)))

tidy_diputados <- diputados %>%
  tidytext::unnest_tokens(word, pdf) %>% 
  filter(!word %in% stopwords) 

pos_neg_words <- tidy_diputados %>%  
  inner_join(lexicon, by = c("word" = "palabra")) %>%
  count(word, sentimiento, sort = TRUE) %>%
  ungroup() %>%
  group_by(sentimiento) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentimiento)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("red2", "green3")) +
  facet_wrap(~sentimiento, scales = "free_y") +
  # ylim(0, 2500) +
  labs(y = NULL, x = NULL) +
  coord_flip() +
  theme_minimal()


tidy_diputados %>%
  inner_join(lexicon, by = c("word" = "palabra")) %>%
  count(word, sentimiento, sort = TRUE) %>% 
  group_by(sentimiento) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(sentimiento = reorder(sentimiento, n)) %>%
  ggplot(aes(sentimiento, n)) +
  geom_col(aes(fill = sentimiento), show.legend = FALSE) +
  scale_fill_manual(values = c("red2", "green3")) +
  ylab(NULL) +
  xlab(NULL) +
  coord_flip() +
  theme_minimal()

stopwords_personalizadas <- c(stopwords_con_numeros, "discusion", "atento",
                              "especial", "dicha", "facultades", "atencion")

tidy_diputados <- diputados %>%
  tidytext::unnest_tokens(word, pdf) %>% 
  filter(!word %in% stopwords_personalizadas) 

tidy_diputados %>%  
  inner_join(lexicon, by = c("word" = "palabra")) %>%
  count(word, sentimiento, sort = TRUE) %>%
  ungroup() %>%
  group_by(sentimiento) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentimiento)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("red2", "green3")) +
  facet_wrap(~sentimiento, scales = "free_y") +
  # ylim(0, 2500) +
  labs(y = NULL, x = NULL) +
  coord_flip() +
  theme_minimal()


tidy_diputados %>%
  inner_join(lexicon, by = c("word" = "palabra")) %>%
  count(word, sentimiento, sort = TRUE) %>% 
  group_by(sentimiento) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(sentimiento = reorder(sentimiento, n)) %>%
  ggplot(aes(sentimiento, n)) +
  geom_col(aes(fill = sentimiento), show.legend = FALSE) +
  scale_fill_manual(values = c("green3", "red2")) +
  ylab(NULL) +
  xlab(NULL) +
  coord_flip() +
  theme_minimal()

# --------- sentiment across months ----------

tidy_diputados  %>%
  inner_join(lexicon, by = c("word" = "palabra")) %>%
  group_by(mes = as.yearmon(fecha)) %>% 
  count(sentimiento) %>%
  ungroup %>% 
  spread(sentimiento, n, fill = 0) %>%
  mutate(sentimiento = pos - neg) %>% 
  arrange(mes) %>% 
  ggplot(aes(as.factor(mes), sentimiento)) +
  # ggplot(aes(as.factor(date), sentimiento)) +
  geom_col(aes(fill = sentimiento > 0), show.legend = FALSE) +
  # facet_grid(.~session) +
  coord_flip() +
  theme_minimal()  +
  scale_fill_viridis(discrete = TRUE)










# marzo - positivo
# - ORDEN DEL DÍA -
#   1º.- Elección de Presidente para el Tercer Período Ordinario de la
# XLVIII Legislatura.
# 2º.- Elección de cuatro Vicepresidentes.
# 3º.- Determinación de días y horas de las sesiones ordinarias.
# 4º.- Fijación de los días destinados al trabajo de las Comisiones. 


# ------------- more extreme sessions -----------------

sesiones_positivas_diputados <- tidy_diputados  %>%
  inner_join(lexicon, by = c("word" = "palabra")) %>%
  group_by(fecha_sesion) %>% 
  count(sentimiento) %>%
  ungroup %>% 
  spread(sentimiento, n, fill = 0) %>%
  mutate(sentimiento = pos - neg) %>% 
  arrange(sentimiento) %>% 
  top_n(2, sentimiento)

# fecha_sesion    neg   pos sentimiento
# <chr>         <dbl> <dbl>       <dbl>
# 1 2017-12-20_1   421.  800.        379. inauguracion de escuela
# 2 2017-08-08_32  278.  660.        382. convenios varios y escuelas y liceos. también salud mental.
# 3 2017-12-12_58  470.  875.        405.

# 1.
# EMILIO VERDESIO. (Designación a la Escuela Especial Nº 133 de Rosario, departamento de Colonia). 
  
sesiones_negativas_diputados <- tidy_diputados  %>%
  inner_join(lexicon, by = c("word" = "palabra")) %>%
  group_by(fecha_sesion) %>% 
  count(sentimiento) %>%
  ungroup %>% 
  spread(sentimiento, n, fill = 0) %>%
  mutate(sentimiento = pos - neg) %>% 
  arrange(sentimiento) %>% 
  top_n(-2, sentimiento)

# fecha_sesion    neg   pos sentimiento
# <chr>         <dbl> <dbl>       <dbl>
# 1 2017-05-10_14  839.  578.       -261.   # venezuela
# 2 2017-02-15_3   815.  703.       -112.   # violencia en el deporte
# 3 2017-06-13_21  187.  101.        -86.

# 1.
# mayo: extraordinaria - negativa
# EVALUACIÓN DEL PODER EJECUTIVO SOBRE LA SITUACIÓN EN LA REPÚBLICA
# BOLIVARIANA DE VENEZUELA A LA LUZ DE LA CARTA DEMOCRÁTICA
# INTERAMERICANA DE LA OEA Y LAS CLÁUSULAS DEMOCRÁTICAS DE LA UNASUR Y
# EL MERCOSUR. (Llamado a Sala al señor Ministro de Relaciones Exteriores).
# (Carp. 1942/017). 

# 2.
# febrero- negativa
# SITUACIÓN DE EXTREMA GRAVEDAD EN MATERIA DE SEGURIDAD PÚBLICA, ESPECIALMENTE
# LOS HECHOS RELACIONADOS CON EL DEPORTE EN LOS ÚLTIMOS TREINTA DÍAS. (Llamado a
# Sala al señor Ministro del Interior). 


# ------------- tf-idf de sesiones extremas ----------

sesiones_extremas <- c(sesiones_negativas_diputados$fecha_sesion, sesiones_positivas_diputados$fecha_sesion)

session_diputados_words <- diputados %>%
  unnest_tokens(word, pdf) %>%
  count(fecha, sesion, fecha_sesion, word, sort = TRUE) %>%
  ungroup()

diputados_words <- session_diputados_words %>% 
  group_by(fecha_sesion) %>% 
  summarize(total = sum(n))

session_diputados_words <- left_join(session_diputados_words, diputados_words)

session_diputados_words <- session_diputados_words %>%
  bind_tf_idf(word, fecha_sesion, n) %>% 
  filter(fecha_sesion %in% sesiones_extremas)

session_diputados_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

session_diputados_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(fecha_sesion) %>% 
  top_n(15, tf_idf) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = fecha_sesion)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~fecha_sesion, ncol = 3,  scales = "free") +
  coord_flip()





# ------------- palabras que más aportan al sentimiento de sesiones extremas ----------

words_sentiment_extreme <- list()

for (i in sesiones_extremas){ 
  words_sentiment_extreme[[i]] <- tidy_diputados  %>%
    inner_join(lexicon, by = c("word" = "palabra")) %>%
    filter(fecha_sesion == i,
           !word %in% c("consideracion", "atencion", "atento", "asunto")) %>% 
    count(fecha, sesion, word, sentimiento, sort = TRUE) %>%
    ungroup() %>%
    group_by(fecha, sesion, sentimiento) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n, fill = sentimiento)) +
    geom_col(show.legend = FALSE) +
    scale_fill_manual(values = c("red2", "green3")) +
    facet_wrap(~sentimiento, scales = "free_y") +
    labs(y = NULL, x = NULL) +
    ggtitle(paste0("Palabras más extremas en la sesión \nde Diputados del ", str_match(i, "[0-9-]+[^\\_]"))) +
    coord_flip() +
    theme_minimal()
}

words_sentiment_extreme[[1]]; words_sentiment_extreme[[2]]
words_sentiment_extreme[[3]]; words_sentiment_extreme[[4]]
