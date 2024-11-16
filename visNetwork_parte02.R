# Cargar las librerías necesarias
library(quanteda)            # Procesamiento de texto
library(quanteda.textplots)  # Visualización de texto
library(stringr)             # Manipulación de cadenas de texto

# Leer y preparar el texto de la novela
texto <- readLines("cien_años_de_soledad.txt", encoding = "UTF-8")
texto <- paste(texto, collapse = " ")  # Unir todo el texto en un solo string
length(texto)                          # Ver la longitud del texto

# Crear un corpus a partir del texto
corpus_cien <- corpus(texto)
corpus_cien |> class()                 # Verificar la clase del objeto corpus

# Tokenización del texto (palabras) eliminando puntuación y números
tokens_all <- tokens(corpus_cien, 
                     what = "word", 
                     remove_punct = TRUE, 
                     remove_numbers = TRUE)

# Convertir a mayúsculas para estandarizar nombres
tokens_all <- tokens_toupper(tokens_all)

# Definir un diccionario de personajes con sus variaciones de nombres
personajes_variaciones <- list(
  'JOSÉ ARCADIO BUENDÍA' = c('JOSÉ ARCADIO BUENDÍA', 'JOSÉ ARCADIO', 'ARCADIO', 'BUENDÍA'),
  'ÚRSULA IGUARÁN' = c('ÚRSULA IGUARÁN', 'ÚRSULA', 'IGUARÁN'),
  'AURELIANO BUENDÍA' = c('AURELIANO BUENDÍA', 'CORONEL AURELIANO BUENDÍA', 'AURELIANO', 'BUENDÍA'),
  # ... (otros personajes y sus variaciones)
  'GERMÁN' = c('GERMÁN')
)

# Crear el diccionario a partir de las variaciones
diccionario_personajes <- dictionary(x = personajes_variaciones)

# Tokenización basada en el diccionario de personajes
tokens_personajes <- tokens_lookup(tokens_all, 
                                   dictionary = diccionario_personajes, 
                                   exclusive = FALSE, 
                                   capkeys = FALSE)

# Filtrar solo los tokens que coinciden con los nombres de personajes
tokens_personajes <- tokens_select(tokens_personajes, 
                                   pattern = names(diccionario_personajes), 
                                   selection = "keep", 
                                   valuetype = "fixed")

# Crear una matriz de co-ocurrencias con un contexto de ventana de 5 palabras
fcm_personajes <- fcm(tokens_personajes, 
                      context = "window", 
                      window = 5, 
                      ordered = FALSE, 
                      tri = TRUE)

# Visualización básica de la red de co-ocurrencias
textplot_network(fcm_personajes, min_freq = 3)

# Convertir la matriz de co-ocurrencias en una tabla de grafos
matriz_ad <- as.matrix(fcm_personajes)
cien_net <- as_tbl_graph(matriz_ad, directed = FALSE)

# Filtrar aristas redundantes y calcular la centralidad de autoridad
cien_net <- cien_net |> 
  activate(edges) |> 
  filter(from != to) |> 
  activate(nodes) |> 
  mutate(centrality = centrality_authority())

# Convertir la red a un formato compatible con visNetwork
grafo_cien <- toVisNetworkData(cien_net)

nodes <- grafo_cien$nodes
edges <- grafo_cien$edges

# Visualización interactiva de la red con visNetwork
visNetwork(nodes, edges, height = "800px", width = "100%") |> 
  visIgraphLayout() |> 
  visNodes(shape = "dot", size = 30,
           color = list(background = "skyblue", bordes = "navy", 
                        highlight = "orange"),
           font = list(size = 30)) |> 
  visEdges(
    shadow = TRUE,
    color = list(color = "#0085AF", highlight = "#C62F4B")
  ) |> 
  visOptions(nodesIdSelection = TRUE,
             highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
             selectedBy = list(variable = "centrality", highlight = TRUE)) |> 
  visInteraction(navigationButtons = TRUE, keyboard = TRUE) |> 
  visLayout(randomSeed = 12)


save.image(file="visNetwork_paqueteSEmana.RData")


save(choco_net, file = "choco_net.rds")
save(cien_net,file="cien_net.rds")
# 
# R version 4.4.1 (2024-06-14 ucrt)
# Platform: x86_64-w64-mingw32/x64
# Running under: Windows 11 x64 (build 26100)
# 
# Matrix products: default
# 
# 
# locale:
# [1] LC_COLLATE=Spanish_Spain.utf8  LC_CTYPE=Spanish_Spain.utf8   
# [3] LC_MONETARY=Spanish_Spain.utf8 LC_NUMERIC=C                  
# [5] LC_TIME=Spanish_Spain.utf8    
# 
# time zone: America/Bogota
# tzcode source: internal
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#  [1] quanteda.textplots_0.94.4 quanteda_4.0.2            tidygraph_1.3.1          
#  [4] visNetwork_2.1.2          lubridate_1.9.3           forcats_1.0.0            
#  [7] stringr_1.5.1             dplyr_1.1.4               purrr_1.0.2              
# [10] readr_2.1.5               tidyr_1.3.1               tibble_3.2.1             
# [13] ggplot2_3.5.1             tidyverse_2.0.0          
# 
# loaded via a namespace (and not attached):
#  [1] utf8_1.2.4           generics_0.1.3       stringi_1.8.3        lattice_0.22-6      
#  [5] statnet.common_4.9.0 hms_1.1.3            digest_0.6.36        magrittr_2.0.3      
#  [9] grid_4.4.1           timechange_0.3.0     pkgload_1.3.4        fastmap_1.2.0       
# [13] jsonlite_1.8.8       Matrix_1.7-0         ggrepel_0.9.5        network_1.18.2      
# [17] stopwords_2.3        fansi_1.0.6          scales_1.3.0         cli_3.6.2           
# [21] rlang_1.1.3          munsell_0.5.1        withr_3.0.0          yaml_2.3.8          
# [25] tools_4.4.1          tzdb_0.4.0           coda_0.19-4.1        colorspace_2.1-0    
# [29] fastmatch_1.1-4      vctrs_0.6.5          R6_2.5.1             lifecycle_1.0.4     
# [33] htmlwidgets_1.6.4    sna_2.7-2            pkgconfig_2.0.3      pillar_1.9.0        
# [37] gtable_0.3.5         glue_1.7.0           Rcpp_1.0.12          tidyselect_1.2.1    
# [41] rstudioapi_0.16.0    farver_2.1.2         htmltools_0.5.8.1    igraph_2.0.3        
# [45] compiler_4.4.1  