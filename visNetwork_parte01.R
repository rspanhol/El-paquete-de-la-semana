
# Limpiar el entorno de trabajo
rm(list = ls())

# Cargar las librerías necesarias
library(tidyverse)   # Manejo de datos
library(visNetwork)  # Visualización de redes
library(tidygraph)   # Manejo de redes en formato tidy

# Cargar la red de datos desde un archivo RDS
load("choc_net.rds")

# Inspeccionar la estructura general del objeto de red
choco_net |> glimpse()

# Mostrar el contenido completo de la red
choco_net

# Convertir la red en un formato compatible con visNetwork
choco_net_data <- toVisNetworkData(choco_net)

# Inspeccionar los datos convertidos
choco_net_data

# Separar los datos de nodos y aristas
nodos <- choco_net_data$nodes
edges <- choco_net_data$edges

# Visualización básica de la red
visNetwork(nodos, edges, height = "800px", width = "100%") |> 
  visNodes(shape = "dot", size = 30,
           color = list(
             background = "beige",
             border = "black",
             highlight = "orange"),
           shadow = list(enabled = TRUE, size = 50),
           font = list(size = 30)) |> 
  visOptions(highlightNearest = TRUE,  # Resalta los nodos cercanos
             nodesIdSelection = TRUE,  # Permite seleccionar nodos por ID
             manipulation = TRUE)      # Habilita la manipulación interactiva

# Renombrar la columna del grupo en los nodos
names(nodos)[2] <- "group"

# Visualización mejorada con grupos y leyenda
visNetwork(nodos, edges, height = "800px", width = "100%") |> 
  visGroups(groupname = "Milk", color = "beige", shape = "square") |> 
  visGroups(groupname = "Dark", color = "brown", shape = "dot") |> 
  visOptions(highlightNearest = TRUE,
             nodesIdSelection = TRUE,
             manipulation = TRUE) |> 
  visLegend() |>                             # Agregar una leyenda
  visInteraction(navigationButtons = TRUE) |> # Botones de navegación
  visLayout(randomSeed = 12)                  # Disposición reproducible

# Ordenar los nodos según su centralidad
choco_net |> 
  activate(nodes) |> 
  arrange(desc(centrality))

# Ajustar el tamaño de los nodos según la centralidad
nodos <- nodos |> mutate(size = centrality * 30)


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
