# Limpiamos el entorno de trabajo eliminando todos los objetos previos
rm(list = ls())

# Cargamos el archivo de datos "choco.RData"
load("choco.RData")

# Importamos las librerías necesarias para análisis de datos y gráficos
library(tidyverse)
library(corrr)  # Para calcular y manipular matrices de correlación
library(tidygraph)  # Para trabajar con gráficos de manera ordenada
library(ggraph)  # Para la visualización de gráficos

# Exploramos las primeras observaciones de los datos cargados
choco |> glimpse()

# Extraemos los nombres únicos de los chocolates y los almacenamos en un vector
nombres <- choco |> distinct(Name) |> pull()


# Exploramos nuevamente la estructura del objeto choco
glimpse(choco)

# Seleccionamos un subconjunto de columnas relevantes para el análisis de correlación
df <- choco[5:14]

# Convertimos los datos seleccionados en un data frame, con los nombres como filas
df <- as.data.frame(df)
rownames(df) <- choco$Name

# Calculamos la matriz de correlación transpuesta, eliminamos valores redundantes y la convertimos en formato largo
cor.choco <- df |> 
  t() |> correlate() |> 
  shave(upper = T) |>  # Remueve valores redundantes en la matriz superior
  stretch(na.rm = T)  # Convierte la matriz en un formato largo, eliminando valores NA

cor.choco

# Convertimos la matriz de correlación en un gráfico de tipo tbl_graph
graph <- as_tbl_graph(cor.choco)


set.seed(777)
ggraph(graph, layout = "fr")+
  geom_edge_link(alpha = 0.5)+  # Dibuja los enlaces entre nodos
  geom_node_point(size = 3.5, col= "brown", 
                  alpha = 0.8)+  # Representa nodos con puntos
  geom_node_text(aes(label = name),
                 repel = T, size = 3, alpha = 0.5)+  # Etiquetas de nodos
  theme_graph()

graph |> autograph()

# Filtramos el gráfico para incluir solo conexiones con alta correlación (r >= 0.998)
sub_graph <- to_subgraph(graph, r >= 0.998, subset_by = "edges")$subgraph
sub_graph

# Visualizamos el subgrafo generado con un layout de fuerza "fr"
set.seed(77)
ggraph(sub_graph, layout = "fr")+
  geom_edge_link()+  # Dibuja los enlaces entre nodos
  geom_node_point(size = 3.5, col= "brown", alpha = 0.8)+  # Representa nodos con puntos
  geom_node_text(aes(label = name), repel = T, 
                 size = 3, alpha = 0.5,
                 max.overlaps =40)+  # Etiquetas de nodos
  theme_graph()  # Aplica un tema limpio para gráficos de nodos

# Ordenamos las correlaciones en orden descendente
sub_graph |> 
  activate(edges) |> 
  arrange(r) |> print(n =20)


# Convertimos el subgrafo en un tibble para su manipulación
df_graph <- as_tibble(sub_graph)
df_graph[c(74,79),]

# Asignamos nombres de filas y columnas a la matriz de distancia
rownames(matrix_dist) <- vec_names
colnames(matrix_dist) <- vec_names

# Creamos un tibble para asociar nombres de chocolates con su tipo
choco.group <- tibble(
  name = choco$Name,
  type = as.factor(choco$Type)
)

# Enriquecemos el subgrafo con información sobre el tipo de cada chocolate
sub_graph <- sub_graph |> 
  activate(nodes) |> 
  left_join(choco.group, by = "name") 

# Renombramos las correlaciones en las aristas como "weight" (peso)
sub_graph <- sub_graph |> 
  activate(edges) |> 
  rename(weight = r)

# Visualizamos nuevamente el subgrafo con ancho de borde proporcional al peso y color por tipo de chocolate
set.seed(777)
ggraph(sub_graph, layout = "fr")+
  geom_edge_link(aes(width = weight), alpha = 0.3)+
  scale_edge_width(range = c(0.2,1))+
  geom_node_point(aes(color = type), size = 3)+
  geom_node_text(aes(label = name), repel = T, size = 3)+
  theme_graph()

# Calculamos y visualizamos la centralidad de autoridad de los nodos
set.seed(777)
sub_graph |> 
  activate(nodes) |> 
  mutate(centrality = centrality_authority()) |> 
  ggraph(layout = "fr")+
  geom_edge_link(aes(width = weight), alpha = 0.3)+
  scale_edge_width(range = c(0.2,1))+
  geom_node_point(aes(color = centrality, size = centrality))+
  geom_node_text(aes(label = name), repel = T, size = 3)+
  scale_color_gradient(low = "yellow", high = "brown")+
  theme_graph()

# Detectamos comunidades en el subgrafo usando el algoritmo infomap
set.seed(777)
sub_graph |> 
  activate(nodes) |> 
  mutate(community = as.factor(group_infomap())) |> 
  ggraph(layout = "fr")+
  geom_edge_link(width = 1, color = "skyblue")+
  geom_node_point(aes(color = community, size = 4))+
  geom_node_text(aes(label = name), repel = T, size = 3)+
  theme_graph()

