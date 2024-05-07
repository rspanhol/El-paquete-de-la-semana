# Limpiar el entorno de trabajo
rm(list=ls())

# Instalación y carga de paquetes necesarios
install.packages("correlation")
library(correlation)
library(tidyverse)

# Lectura de datos desde un archivo CSV
wines <- read_csv("https://raw.githubusercontent.com/rspanhol/El-paquete-de-la-semana/main/wine.csv")

# Exploración inicial de los datos
glimpse(wines)
dim(wines)

# Conversión de la variable Wine a factor con etiquetas personalizadas
wines$Wine <- factor(wines$Wine, labels = paste("Type", 1:3, sep = "_"))

# Carga del paquete janitor y creación de una tabla de frecuencias para la variable Wine
library(janitor)
wines |> tabyl(Wine)

# Cálculo de la matriz de correlación para el dataset completo
resultados <- wines |> correlation()

# Visualización de la matriz de correlación con un tema gráfico vacío
resultados |> 
  summary() |> 
  plot() +
  theme_void()

# Filtrado y visualización de correlaciones fuertes (r >= 0.6)
resultados |> 
  filter(r >= 0.6) |> 
  summary() |> 
  plot()

# Cálculo y visualización de la correlación para el tipo de vino 'Type_3'
wines |> 
  filter(Wine == "Type_3") |>
  correlation() |> 
  summary() |> 
  plot()

# Aplicación de la prueba de Shapiro-Wilk para normalidad a todas las columnas numéricas
lapply(wines[-1], shapiro.test)

# Cálculo y resumen de la correlación de Spearman
wines |> 
  correlation(method = "spearman") |> 
  summary()

# Cálculo y visualización de la correlación de Kendall
wines |> 
  correlation(method = "kendall") |> 
  summary() |>
  plot()

# Visualización de correlaciones por grupos de vino en el entorno de RStudio
wines |> 
  group_by(Wine) |> 
  correlation() |> 
  summary() |> View()
