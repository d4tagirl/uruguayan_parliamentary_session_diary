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
library(stringi)

diputados <- readRDS("data/pdf_diputados")
senadores <- readRDS("data/pdf_senadores")

# me deshago de los saltos de línea que sólo los voy a usar más adelante
diputados <- diputados %>%
  mutate(pdf = stri_replace_all(pdf, replacement = "", regex = "\\\n"))

senadores <- senadores %>%
  mutate(pdf = stri_replace_all(pdf, replacement = "", regex = "\\\n"))


# --- Lexicon: http://web.eecs.umich.edu/~mihalcea/downloads/SpanishSentimentLexicons.tar.gz

# comentarios acerca del Lexicon:
# No hice un análisis profundo del lexicon, pero tiene evidentes limitaciones:
#    * Tiene muy pocos términos (476 positivas de 871 en total)
#    * La mayoría (si no todos) los adjetivos que considera son masculinos

lexicon <- read_tsv("SpanishSentimentLexicons/fullStrengthLexicon.txt",
                    col_names = FALSE) %>% 
  select(X1, X3) %>% 
  rename(palabra = X1, sentimiento = X3)

stopwords <- corpora("words/stopwords/es")$stopWords

# agrego a las stopwords números que se repiten mucho
stopwords <- c(stopwords, as.character(seq(1:50)))

# separo en palabras filtrando las stopwords
tidy_diputados <- diputados %>%
  tidytext::unnest_tokens(word, pdf) %>% 
  filter(!word %in% stopwords) 

# agrego sentimiento y visualizo
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

# veo cantidad de palabras positivas y negativas que hay
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

# como hay algunas palabras cuya clasificación no me convence, las excluyo

stopwords_personalizadas <- c(stopwords, "negro", "discusion", "atento", "consideracion",
                              "especial", "dicha", "facultades", "atencion", "asunto")

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
  scale_fill_manual(values = c("red2", "green3")) +
  ylab(NULL) +
  xlab(NULL) +
  coord_flip() +
  theme_minimal()

# --------- sentimiento a lo largo de los meses ----------

tidy_diputados  %>%
  inner_join(lexicon, by = c("word" = "palabra")) %>%
  group_by(mes = as.yearmon(fecha)) %>% 
  count(sentimiento) %>%
  ungroup %>% 
  spread(sentimiento, n, fill = 0) %>%
  mutate(sentimiento = pos - neg) %>% 
  arrange(mes) %>% 
  ggplot(aes(as.factor(mes), sentimiento)) +
  geom_col(aes(fill = sentimiento > 0), show.legend = FALSE) +
  theme_minimal() +
  theme(axis.title.x=element_blank(), axis.text.x  = element_text(angle=45,
                                                                  hjust = 1,
                                                                  vjust = 1)) +
  scale_fill_viridis(discrete = TRUE)

# Nota: en Setiembre Renunció el Vice Presidente 


# ------------- Sesiones más extremas -----------------

sesiones_positivas_diputados <- tidy_diputados  %>%
  inner_join(lexicon, by = c("word" = "palabra")) %>%
  group_by(fecha_sesion) %>% 
  count(sentimiento) %>%
  ungroup %>% 
  spread(sentimiento, n, fill = 0) %>%
  mutate(sentimiento = pos - neg) %>% 
  arrange(sentimiento) %>% 
  top_n(2, sentimiento)

  #   fecha_sesion   neg   pos sentimiento
  #   <chr>        <dbl> <dbl>       <dbl>
  # 1 2017-12-20_1  336.  506.        170.  inauguracion de escuela
  # 2 2018-03-06_2  158.  379.        221.  día de la mujer, etc.

# diputados$pdf[diputados$fecha_sesion == "2017-12-20_1"]
# diputados$pdf[diputados$fecha_sesion == "2018-03-06_2"]

sesiones_negativas_diputados <- tidy_diputados  %>%
  inner_join(lexicon, by = c("word" = "palabra")) %>%
  group_by(fecha_sesion) %>% 
  count(sentimiento) %>%
  ungroup %>% 
  spread(sentimiento, n, fill = 0) %>%
  mutate(sentimiento = pos - neg) %>% 
  arrange(sentimiento) %>% 
  top_n(-2, sentimiento)

  #   fecha_sesion    neg   pos sentimiento
  #   <chr>         <dbl> <dbl>       <dbl>
  # 1 2017-05-10_14  792.  360.       -432.  # interpelación Nin Novoa - Venezuela
  # 2 2017-02-15_3   788.  484.       -304.  # interpelación Bonomi - violencia en el deporte


# ------------- tf-idf de sesiones extremas de diputados ----------

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
