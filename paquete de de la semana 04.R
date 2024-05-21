# Elimina todos los objetos del entorno de trabajo
rm(list=ls())

# Carga la librería tidyverse para manipulación de datos
library(tidyverse)

# Instala el paquete gtsummary si no está instalado
# install.packages("gtsummary")

# Carga la librería gtsummary para crear tablas de resumen
library(gtsummary)

# Muestra el dataset 'trial' incluido en la librería gtsummary
trial

# Muestra la documentación del dataset 'trial'
?trial

# Crea una tabla de resumen del dataset 'trial' sin considerar valores faltantes
trial |> 
  tbl_summary(
    missing = "no"
  )

# Crea una tabla de resumen del dataset 'trial', agrupando por la variable 'trt', sin considerar valores faltantes
trial |> 
  tbl_summary(
    by = trt,
    missing = "no"
  )

# Selecciona variables específicas y crea una tabla de resumen, agrupando por 'trt', sin considerar valores faltantes, y agrega columnas con los datos globales y los p-valores
trial |> select(trt, age, marker, response, death) |> 
  tbl_summary(
    by = trt,
    missing = "no"
  ) |> 
  add_overall() |> 
  add_p()

# Selecciona variables específicas y crea una tabla de resumen, agrupando por 'trt', con estadísticas específicas para 'age' y 'marker', sin considerar valores faltantes, y agrega columnas con los datos globales y los p-valores
trial |> select(trt, age, marker, response, death) |> 
  tbl_summary(
    by = trt,
    statistic = list(
      age ~ c("{mean} ({sd})"),
      marker ~ c("{mean} ({sd})")
    ),
    missing = "no"
  ) |> 
  add_overall() |> 
  add_p()

# Crea un modelo de regresión logística con 'response' como variable dependiente y 'age', 'grade' y 'marker' como variables independientes
mod1 <- glm(response ~ age + grade + marker, data = trial,
            family = binomial)

# Muestra el resumen del modelo de regresión
summary(mod1)

# Crea una tabla de resumen de la regresión con los coeficientes exponenciados
mod1 |> 
  tbl_regression(exponentiate = TRUE)

# Crea una tabla de regresión univariable para varias variables, con 'response' como variable dependiente, utilizando un modelo de regresión logística, y agrega el número de eventos y el p-valor global
trial |> 
  select(response, age, grade, marker) |> 
  tbl_uvregression(
    y = response,
    method = glm,
    method.args = list(family = binomial),
    exponentiate = TRUE,
    pvalue_fun = ~style_pvalue(., digits = 2)
  ) |> 
  add_nevent() |> 
  add_global_p()

# Aplica el tema 'nejm' a las tablas de resumen
theme_gtsummary_journal("nejm")

# Crea una tabla de regresión univariable con el tema 'nejm' aplicado, para varias variables, con 'response' como variable dependiente, utilizando un modelo de regresión logística, y agrega el número de eventos y el p-valor global
trial |> 
  select(response, age, grade, marker) |> 
  tbl_uvregression(
    y = response,
    method = glm,
    method.args = list(family = binomial),
    exponentiate = TRUE,
    pvalue_fun = ~style_pvalue(., digits = 2)
  ) |> 
  add_nevent() |> 
  add_global_p()

