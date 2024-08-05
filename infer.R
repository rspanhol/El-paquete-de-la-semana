# Elimina todos los objetos en el entorno de trabajo actual
rm(list = ls())

# Carga las bibliotecas necesarias para el análisis
library(tidyverse)
library(infer)
library(skimr)

# Muestra la documentación del conjunto de datos 'gss'
?gss

# Proporciona una vista general de la estructura del conjunto de datos 'gss'
glimpse(gss)

# Otra forma de obtener una vista general de la estructura del conjunto de datos 'gss'
gss |> glimpse()

# Resumen descriptivo del conjunto de datos 'gss'
skim(gss)

# Calcular el valor observado de la media de la variable 'hours'
valor_obs <- gss |> 
  specify(response = hours) |> 
  calculate(stat = "mean")
valor_obs

# Generar la distribución nula utilizando bootstrap
dist_nula <-  gss |> 
  specify(response = hours) |> 
  hypothesise(null = "point", mu = 40) |> 
  generate(reps = 1000, type = "bootstrap") |> 
  calculate(stat = "mean")

# Visualizar la distribución nula y sombrear el valor p
dist_nula |> 
  visualise() +
  shade_p_value(valor_obs, direction = "two-sided")

# Obtener el valor p a partir de la distribución nula
dist_nula |> 
  get_p_value(valor_obs, direction = "two-sided")

# Realizar una prueba t para la variable 'hours' con media hipotética 40
t_test(gss, response = hours, mu  = 40)

# Crear un diagrama de cajas para 'hours' categorizado por 'college'
gss |> 
  ggplot(aes(x = college, y = hours)) +
  geom_boxplot()

# Calcular la diferencia observada en las medias de 'hours' entre 'degree' y 'no degree'
valor_obs_two <- gss |> 
  specify(hours ~ college) |> 
  calculate(stat = "diff in means", order = c("degree", "no degree"))
valor_obs_two

# Generar la distribución nula utilizando permutaciones
dist_nula_two <- 
  gss |> 
  specify(hours ~ college) |> 
  hypothesise(null = "independence") |> 
  generate(reps = 1000, type = "permute") |> 
  calculate(stat = "diff in means", order = c("degree", "no degree"))

# Visualizar la distribución nula y sombrear el valor p para la diferencia en medias
dist_nula_two |> 
  visualise() +
  shade_p_value(valor_obs_two, direction = "two-sided")

# Obtener el valor p para la diferencia en medias a partir de la distribución nula
dist_nula_two |> 
  get_p_value(obs_stat = valor_obs_two, direction = "two-sided")

# Realizar una prueba t para la diferencia en medias de 'hours' entre 'degree' y 'no degree'
t_test(
  x = gss,
  formula = hours ~ college,
  order = c("degree", "no degree"),
  alternative = "two-sided"
)

