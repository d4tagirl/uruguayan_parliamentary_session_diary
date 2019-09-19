extract_pdf <- function(url, pag = 1){
  
  #armo la url de la página 2
  if (pag != 1){
    url <- url %>%
      read_html() %>%
      html_nodes(".pager-item a") %>%
      html_attr("href") %>%
      map(~ paste0("https://parlamento.gub.uy", .)) %>%
      unlist() %>% 
      .[pag-1]
  }
  GESTIÓN DE ASSE DESDE EL AÑO 2008 HASTA LA FECHA Y LA DENOMINADA ESTAFA AL FONASA. 
  pdfs <- url %>%
    read_html() %>%
    html_nodes(".views-field-DS-File-IMG a") %>%   # seleccionar clase
    html_attr("href") %>%
    map(~ paste0("https://parlamento.gub.uy", .)) %>% 
    map(~ paste0(pdf_text(.), collapse = ' ')) %>%
    map(~ stri_trans_general(tolower(.), id = "latin-ascii")) %>%
  #   map(~ stri_replace_all(., replacement = "", regex = "\\\n")) %>% # voy a dejar esto así puedo después quedarme con renglones!
    map_df(function(pdf) {tibble(pdf)})
  
  return(pdfs)
}

extract_metadata <- function(url, info, pag = 1){
  if (info == "fecha") nodes = "td.views-field-DS-Fecha"
  if (info == "sesion") nodes = "td.views-field-Ssn-Nro"
  if (pag != 1){
    url <- url %>%
      read_html() %>%
      html_nodes(".pager-item a") %>%
      html_attr("href") %>%
      map(~ paste0("https://parlamento.gub.uy", .)) %>%
      unlist() %>% 
      .[pag-1]
    }
  
  url %>% 
    read_html() %>% 
    html_nodes(nodes) %>% 
    html_text() %>% 
    map(~str_extract(., "[0-9\\-]+")) %>% 
    unlist()
}

  