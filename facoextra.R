# Limpia el entorno de trabajo eliminando todos los objetos.
rm(list = ls())

# Carga las librerías necesarias: tidyverse para la manipulación y visualización de datos,
# FactoMineR para análisis multivariado, y factoextra para visualización de resultados de análisis multivariado.
library(tidyverse)
library(FactoMineR)
library(factoextra)

# Define la URL de un archivo CSV con datos sobre chocolates.
"https://raw.githubusercontent.com/schloerke/cranvasOLD/master/files/data/chocolates.csv"

# Lee los datos desde la URL proporcionada y los guarda en un dataframe llamado 'choco'.
choco <- read_csv("https://raw.githubusercontent.com/schloerke/cranvasOLD/master/files/data/chocolates.csv")

# Muestra una vista rápida de la estructura y los tipos de datos del dataframe 'choco'.
glimpse(choco)

# Crea un nuevo dataframe 'data' eliminando las primeras cuatro columnas de 'choco'.
data <- choco[-c(1:4)]

# Transforma el dataframe a un formato largo para que cada variable de interés (Calories, Fat, Carbohydrates, Protein)
# esté en su propia fila, luego se genera un boxplot para cada variable.
data |> 
  pivot_longer(cols = Calories:Protein,
               names_to = "var",
               values_to = "valor") |> 
  ggplot(aes(x = factor(var), y = valor, fill = factor(var))) +
  geom_boxplot() +
  theme_classic() +
  facet_wrap(~var, scales = "free")

# Escala los datos en el dataframe 'data' para normalizar las variables, y los guarda en 'data_scaled'.
data_scaled <- scale(data)

# Realiza un Análisis de Componentes Principales (PCA) sobre los datos escalados.
pca1 <- princomp(data_scaled)

# Muestra un resumen del PCA, incluyendo la varianza explicada por cada componente.
summary(pca1)

# Extrae las coordenadas de los componentes principales.
get_pca(pca1)$coord

# Visualiza un biplot del PCA, que muestra tanto las variables como las observaciones en el espacio de los primeros dos componentes principales.
fviz_pca_biplot(pca1)

# Visualiza la contribución de cada variable a los primeros tres componentes principales.
fviz_pca_contrib(pca1, choice= "var", axes = 1)
fviz_pca_contrib(pca1, choice= "var", axes = 2)
fviz_pca_contrib(pca1, choice= "var", axes = 3)

# Genera un biplot del PCA mostrando solo los puntos (observaciones).
fviz_pca_biplot(pca1, geom = "point")

# Visualiza las variables en el espacio de los componentes principales, mostrando los nombres de las variables.
fviz_pca_var(pca1, geom = "text")

# Realiza otro PCA incluyendo las primeras cuatro columnas del dataframe original como variables cualitativas adicionales.
pca2 <- PCA(choco, quali.sup = 1:4)

# Genera un biplot del segundo PCA, coloreando las observaciones por el tipo de chocolate y sin añadir elipses alrededor de los grupos.
fviz_pca_biplot(pca2, habillage = "Type", addEllipses = F)

# Realiza nuevamente el PCA con las mismas variables cualitativas, y genera un biplot con elipses alrededor de los grupos.
ca2 <- PCA(choco, quali.sup = 1:4)
fviz_pca_biplot(pca2, habillage = "Type", addEllipses = T)

# Realiza un análisis de clustering jerárquico en los datos escalados, utilizando 3 clusters.
mi_clust <- eclust(data, FUNcluster = "hclust", k = 3)

# Visualiza el dendrograma resultante del clustering jerárquico.
fviz_dend(mi_clust)

# Determina el número óptimo de clusters utilizando el método del codo (WSS) y el método del silhouette.
fviz_nbclust(data_scaled, hcut, method = "wss")
fviz_nbclust(data_scaled, hcut, method = "silhouette")

# Realiza un análisis de clustering con el algoritmo k-means, usando 2 centros (clusters).
kmedias <- kmeans(data_scaled, centers = 2)

# Visualiza los resultados del clustering k-means.
fviz_cluster(kmedias, data = data_scaled)

# Determina el número óptimo de clusters para k-means usando el método del silhouette.
fviz_nbclust(data_scaled, kmeans, method = "silhouette")

# Añade la clasificación por clusters como una nueva columna en el dataframe original.
data$cluster <- factor(kmedias$cluster)
