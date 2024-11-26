# Limpieza del entorno de trabajo
rm(list = ls())

# Carga de paquetes necesarios
library(tidyverse)  # Paquete para manipulación de datos y gráficos
library(tidyplots)  # Paquete para visualizaciones simplificadas y personalizables

# Visualización inicial de los datos
head(study)  # Muestra las primeras filas del dataset "study"

# Ejemplo 1: Gráfico básico con barras de medias, errores estándar y puntos de datos
study |> 
  tidyplot(x = treatment, y = score, color = dose) |>  # Configuración inicial de ejes y color
  add_mean_bar(alpha = 0.3) |>  # Agrega barras que representan la media
  add_sem_errorbar() |>  # Agrega barras de error estándar
  add_data_points() |>  # Superpone los puntos individuales de los datos
  adjust_size(width = NA, height = NA)  # Ajusta el tamaño del gráfico

# Comparación: Ejemplo equivalente usando ggplot2
# Este código está comentado para mostrar cómo se haría de forma tradicional.
# study |> 
#   ggplot(aes(group, score, fill = dose, color = dose)) +
#   stat_summary(geom = "bar", fun = mean, position = position_dodge(width = 0.8),
#                alpha = 0.3, color = NA, width = 0.6) +
#   stat_summary(geom = "errorbar", fun.data = mean_se,
#                position = position_dodge(width = 0.8), width = 0.4) +
#   geom_point(position = position_dodge(width = 0.8)) +
#   theme_classic()

# Ejemplo 2: Gráfico con barras, líneas de medias, valores numéricos y puntos agrupados
study |> 
  tidyplot(x = treatment, y = score, color = dose) |> 
  add_mean_bar(alpha = 0.3) |>  # Agrega barras de medias
  add_mean_dash() |>  # Agrega líneas horizontales para indicar la media
  add_mean_value() |>  # Agrega valores de la media sobre las barras
  add_data_points_beeswarm() |>  # Muestra los puntos distribuidos como "beeswarm"
  adjust_size(width = NA, height = NA)  # Ajusta el tamaño

# Carga de otro conjunto de datos y exploración inicial
load("happy_world.rds")  # Carga un dataset llamado "happy_world"
happy_world |> glimpse()  # Inspección rápida del contenido del dataset

# Ejemplo 3: Boxplot de índices de felicidad por región
happy_world |> filter(year == 2021) |>  # Filtra datos para el año 2021
  tidyplot(x = region, y = hpi, color = region) |>  # Configura el eje x, eje y y colores por región
  add_boxplot() |>  # Agrega un boxplot
  add_data_points() |>  # Superpone los puntos individuales
  adjust_size(width = NA, height = NA)  # Ajusta el tamaño del gráfico

# Exploración inicial del dataset de expresión génica
gene_expression |> head()  # Muestra las primeras filas del dataset

# Ejemplo 4: Heatmap de expresión génica
gene_expression |> 
  tidyplot(x = sample, y = external_gene_name, color = expression) |>  # Configura ejes y color por expresión
  add_heatmap(scale = "row", rasterize = F, rasterize_dpi = 50) |>  # Agrega un heatmap escalado por fila
  adjust_size(width = NA, height = NA)  # Ajusta el tamaño

# Ejemplo 5: Heatmap dividido por condición experimental
gene_expression |> 
  tidyplot(x = sample, y = external_gene_name, color = expression) |> 
  add_heatmap(scale = "row", rasterize = F, rasterize_dpi = 50) |> 
  split_plot(by = condition, widths = NA, heights = NA)  # Divide el gráfico por condiciones

# Ejemplo 6: Gráfico con puntos y barras dividido por sexo
study |> 
  tidyplot(x = treatment, y = score, color = dose) |> 
  add_mean_bar(alpha = 0.3) |>  # Agrega barras de medias
  add_mean_dash() |>  # Agrega líneas horizontales
  add_mean_value() |>  # Agrega valores numéricos de la media
  add_data_points_beeswarm() |>  # Muestra puntos distribuidos como "beeswarm"
  split_plot(by = sex, widths = NA, heights = NA)  # Divide el gráfico por sexo

