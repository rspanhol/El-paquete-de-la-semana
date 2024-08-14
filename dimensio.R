# Limpiar el entorno de trabajo eliminando todos los objetos
rm(list = ls())

# Definir una lista de paquetes necesarios
paquetes <- c("scatterplot3d", "tessellation", "dimensio")

# Instalar los paquetes definidos si no están ya instalados
install.packages(paquetes)

# Cargar las bibliotecas necesarias para el análisis
library(dimensio)
library(tessellation)
library(scatterplot3d)
library(tidyverse)

# Crear un conjunto de datos utilizando la función 'teapot' del paquete 'tessellation'
teapot <- teapot()

# Mostrar las primeras filas del conjunto de datos 'teapot'
head(teapot)

# Crear un gráfico 3D del conjunto de datos 'teapot' utilizando 'scatterplot3d'
scatterplot3d(teapot[,1], teapot[,2], teapot[,3],
              highlight.3d = TRUE,  # Resaltar los puntos en 3D
              angle = 90, pch = 19, xlab = "", ylab = "",
              main = "Jarra de Té")

# Realizar un Análisis de Componentes Principales (PCA) en el conjunto de datos 'teapot'
modelo <- pca(teapot, center = TRUE, scale = TRUE)

# Resumir los resultados del PCA
summary(modelo)

# Obtener y visualizar las contribuciones de las variables en el PCA
get_contributions(modelo)
viz_contributions(modelo)

# Crear un scree plot para visualizar las varianzas explicadas por cada componente principal
screeplot(modelo)

# Visualizar las contribuciones nuevamente (repetido en el código original)
viz_contributions(modelo)
viz_contributions(modelo)

# Crear un biplot del modelo PCA, mostrando las variables y las observaciones
biplot(modelo, type = "form")

# Obtener el directorio de trabajo actual
getwd()

# Leer los datos de pizza desde un archivo CSV
pizza <- read_csv("Pizza.csv")
pizza

# Limpiar el conjunto de datos 'pizza' eliminando la segunda columna
pizza_clean <- pizza[-2]

# Convertir las columnas de tipo 'character' a factores
pizza_clean <- pizza_clean |> 
  mutate_if(is.character, as.factor)

# Resumir la variable 'brand' del conjunto de datos limpio
pizza_clean$brand |> summary()

# Realizar un Análisis de Componentes Principales (PCA) en el conjunto de datos 'pizza_clean'
pca_pizza <- pca(pizza_clean, center = TRUE, scale = TRUE,
                 sup_quali = 1)  # Considerar la primera columna como suplementaria cualitativa

# Resumir los resultados del PCA para el conjunto de datos 'pizza_clean'
summary(pca_pizza)

# Mostrar la clase del objeto 'pca_pizza'
class(pca_pizza)

# Crear un scree plot para visualizar las varianzas explicadas por cada componente principal en 'pca_pizza'
screeplot(pca_pizza, eigenvalues = FALSE, cumulative = TRUE)

# Crear un biplot del modelo PCA para 'pca_pizza'
biplot(pca_pizza)

# Cargar la paleta de colores de 'RColorBrewer'
library(RColorBrewer)
mis_colores <- brewer.pal(10, name = "Paired")

# Visualizar las observaciones individuales del PCA, destacando la variable 'brand'
viz_individuals(x = pca_pizza,
                highlight = pizza$brand,
                color = mis_colores, pch = 20, cex = 3,
                legend = list(x = "topright", inset = c(-0.2, 0), xpd = TRUE,
                              pt.cex = 0.8, cex = 0.8), sup = TRUE, active = TRUE)

