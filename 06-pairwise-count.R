# Acá mi intención era ver qué palabras rodeaban ciertas palabras clave, como Sendic o ANCAP. Para eso necesitaba
# definir unidades intermedias entre un documento entero (la sesión) y la palabra, entonces probé separar por renglones
# después por grupos de renglones. La idea es ver si hay palabras que se usen en el mismo renglón o grupo de renglones
# más frecuentemente que otras. Para eso usé el pairwise_count.

# no fue usado en el artículo

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

# ----- separo por grupos de renglones -------

# Diputados

stopwords <- corpora("words/stopwords/es")$stopWords

stopwords <- c(stopwords, as.character(seq(1:50)))

renglones_diputados <- diputados %>%
  tidytext::unnest_tokens(renglon, pdf, token = stringr::str_split, pattern = "\\\n") %>% 
  mutate(renglon = str_replace_all(renglon, "[\\s]+", " "),
         section = row_number() %/% 100) %>%      # secciones de 100 renglones
  group_by(section) %>% 
  summarise(text = paste0(renglon, collapse = " ")) %>% 
  # quiero quedarme con los grupos de renglones que cumplen con la expresión regular:
  # siempre que se diga ancap en algún momento y en algún momento posterior dice sendic, o viceversa.
  filter(str_detect(text, "ancap.*sendic|sendic.*ancap")) %>%  
  tidytext::unnest_tokens(word, text) %>% 
  filter(!word %in% stopwords) 

library(widyr)
diputados_pair_count <- renglones_diputados %>%
  widyr::pairwise_count(word, section, sort = TRUE) 

# visualizo con un grafo

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

# no lo uso en el artículo

# Senadores

renglones_senadores <- senadores %>%
  tidytext::unnest_tokens(renglon, pdf, token = stringr::str_split, pattern = "\\\n") %>% 
  mutate(renglon = str_replace_all(renglon, "[\\s]+", " "),
         section = row_number() %/% 100) %>%      #secciones de 100 renglones
  group_by(section) %>% 
  summarise(text = paste0(renglon, collapse = " ")) %>% 
  filter(str_detect(text, "ancap.*sendic|sendic.*ancap")) %>% 
  tidytext::unnest_tokens(word, text) %>% 
  filter(!word %in% stopwords) 

library(widyr)
senadores_pair_count <- renglones_senadores %>%
  widyr::pairwise_count(word, section, sort = TRUE) 

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
