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
library(stringr)

diputados <- readRDS("data/pdf_diputados")
senadores <- readRDS("data/pdf_senadores")

# ----- separo renglones -------

# palabra_clave <- "renuncia"
# palabras_clave <- c("renuncia", "ancap", "sendic")

# Diputados

stopwords <- corpora("words/stopwords/es")$stopWords

stopwords <- c(stopwords, as.character(seq(1:50)))

renglones_diputados <- diputados %>%
  tidytext::unnest_tokens(renglon, pdf, token = stringr::str_split, pattern = "\\\n") %>% 
  mutate(renglon = str_replace_all(renglon, "[\\s]+", " "),
         section = row_number() %/% 100) %>%      #secciones de 15 renglones
  # filter(str_detect(renglon, palabra_clave)) %>% 
  group_by(section) %>% 
  summarise(text = paste0(renglon, collapse = " ")) %>% 
  filter(str_detect(text, "ancap.*sendic|sendic.*ancap")) %>% 
  tidytext::unnest_tokens(word, text) %>% 
  filter(!word %in% stopwords) 

library(widyr)
diputados_pair_count <- renglones_diputados %>%
  widyr::pairwise_count(word, section, sort = TRUE) 

# renglones_diputados %>% 
#   group_by(word) %>%
#   filter(n() >= 15) %>%
#   pairwise_cor(word, section, sort = TRUE) %>%
#   filter(item1 %in% palabras_clave)

library(igraph)
library(ggraph)
set.seed(1234)
diputados_pair_count %>%
  filter(
    # item1 %in% palabras_clave,
         n >= 4) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()



# Senadores

renglones_senadores <- senadores %>%
  tidytext::unnest_tokens(renglon, pdf, token = stringr::str_split, pattern = "\\\n") %>% 
  mutate(renglon = str_replace_all(renglon, "[\\s]+", " "),
         section = row_number() %/% 100) %>%      #secciones de 15 renglones
  # filter(str_detect(renglon, palabra_clave)) %>% 
  group_by(section) %>% 
  summarise(text = paste0(renglon, collapse = " ")) %>% 
  filter(str_detect(text, "ancap.*sendic|sendic.*ancap")) %>% 
  tidytext::unnest_tokens(word, text) %>% 
  filter(!word %in% stopwords) 

library(widyr)
senadores_pair_count <- renglones_senadores %>%
  widyr::pairwise_count(word, section, sort = TRUE) 

# renglones_senadores %>% 
#   group_by(word) %>%
#   filter(n() >= 15) %>%
#   pairwise_cor(word, section, sort = TRUE) %>%
#   filter(item1 == palabra_clave)

library(igraph)
library(ggraph)
set.seed(1234)
senadores_pair_count %>%
  filter(
    # item1 %in% palabras_clave,
    n >= 4) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()
