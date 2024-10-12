# Limpiar el entorno de trabajo eliminando todos los objetos
rm(list=ls())

# Cargar de los paquetes necesarios para el análisis
library(tidyverse)        # Para manipulación y visualización de datos
library(FactoMineR)      # Para realizar Análisis de Correspondencias Múltiples (MCA)
library(factoextra)      # Para visualización de resultados de análisis
library(skimr)           # Para obtener un resumen de los datos
library(compareGroups)   # Para comparar grupos en los datos

# Carga el conjunto de datos 'tea'
data(tea)

# Mostrar las dimensiones del conjunto de datos 'tea'
dim(tea)

# Mostrar una visión general de los datos utilizando 'glimpse'
tea |> glimpse()

# Realizar un Análisis de Correspondencias Múltiples (MCA)
# Se especifican las variables cuantitativas y cualitativas
res.MCA <- MCA(tea, quanti.sup = 19, quali.sup = 20:36,
               graph = FALSE, method = "Burt")

#Mostar los resultados del análisis MCA
res.MCA

# Visualizar las variables del análisis MCA
fviz_mca_var(res.MCA)

# Visualizar los individuos del análisis MCA usando puntos
fviz_mca_ind(res.MCA, geom = "point")

# Realizar un análisis de clustering jerárquico sobre los resultados del MCA
res.HCPC <- HCPC(res = res.MCA)

# Visualizar los clusters obtenidos del análisis de clustering
fviz_cluster(res.HCPC, ggtheme = theme_classic())

# Visualizar el dendrograma de los clusters en un formato filogenético
fviz_dend(res.HCPC, type = "phylogenic")

# Añadir la información de los clusters al conjunto de datos 'tea'
tea$cluster <- res.HCPC$data.clust$clust

# Mostrar una visión general del conjunto de datos 'tea' con la nueva variable de cluster
glimpse(tea)

# Crear una tabla descriptiva que compara las variables por clúster
descrTable(cluster ~., data = tea)
```
