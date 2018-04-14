# Diarios de Sesiones de Diputados y Senadores uruguayas: scraping y text mining

Éste es el código que usé para [este artículo de mi blog](https://d4tagirl.com/2018/04/scrapeando-las-sesiones-parlamentarias-de-uruguay) donde scrapeo los Diarios de Sesiones de Diputados y Senadores de Uruguay desde enero de 2017 hasta marzo de 2018, y de [este otro artículo donde analizo el texto de las sesiones](https://d4tagirl.com/2018/04/de-qu%C3%A9-se-habl%C3%B3-en-el-parlamento-uruguayo-desde-2017)

## Archivos disponibles en csv (desde enero de 2017 hasta marzo de 2018):

  * [Diputados](https://github.com/d4tagirl/uruguayan_parliamentary_session_diary/raw/master/data/diputados.csv)
  * [Senadores](https://github.com/d4tagirl/uruguayan_parliamentary_session_diary/raw/master/data/senadores.csv)

## Paquetes destacados:

  * `robotstxt` de [rOpenSci](https://ropensci.org/), para ver si la sección del sitio web que quiero navegar permite ser accedida por un robot 🤖;
  * `rvest`, para explorar la web y descargar los Diarios de Sesiones;
  * `pdftools` también de rOpenSci, para extraer el contenido de los archivos en formato pdf;
  * `tidytext` para analizar el texto de una manera _tidy_.

## Lexicon de sentimiento

Utilicé [este lexicon de sentimiento](http://web.eecs.umich.edu/~mihalcea/downloads/SpanishSentimentLexicons.tar.gz) en mi análisis. No hay muchas opciones disponibles para el idioma español, y los resultados obtenidos tenían sentido. 

No hice un análisis profundo del lexicon, pero tiene evidentes limitaciones:
  * tiene muy pocos términos (476 positivas de 871 en total);
  * la mayoría (si no todos) los adjetivos que considera son masculinos.
  
## Contenido:

  * Scraping.
  * Frecuencia y largo de las sesiones de Diputados y Senadores.

![Frecuencia de las sesiones de Diputados y Senadores](https://github.com/d4tagirl/uruguayan_parliamentary_session_diary/blob/master/images/histograma-1.png)
![Largo de las sesiones de Diputados y Senadores](https://github.com/d4tagirl/uruguayan_parliamentary_session_diary/blob/master/images/unnamed-chunk-1-1.png)

  * Palabras más usadas en las sesiones de ambas Cámaras.
  * Palabras más usadas con sentimiento negativo y positivo, en ambas cámaras.

![Palabras más usadas con sentimiento negativo y positivo, en ambas cámaras](https://github.com/d4tagirl/uruguayan_parliamentary_session_diary/blob/master/images/unnamed-chunk-4-1.png) 
 
  * Análisis de sentimiento por mes y en cada sesión, para ambas Cámaras.
 
![Análisis de sentimiento por mes y en cada sesión, para ambas Cámaras](https://github.com/d4tagirl/uruguayan_parliamentary_session_diary/blob/master/images/unnamed-chunk-6-1.png) 

  * Análisis de temas tratados en ambas cámaras, mediante el cálculo del tf-idf.
  
![Análisis de temas tratados en Diputados](https://github.com/d4tagirl/uruguayan_parliamentary_session_diary/blob/master/images/unnamed-chunk-13-1.png)   

![Análisis de temas tratados en Senadores](https://github.com/d4tagirl/uruguayan_parliamentary_session_diary/blob/master/images/unnamed-chunk-14-1.png)  


