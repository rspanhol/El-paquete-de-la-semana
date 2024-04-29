# Limpieza de todos los objetos en el entorno de R para iniciar el análisis con una sesión limpia
rm(list=ls())

# Carga de las bibliotecas necesarias. Si 'compareGroups' no está instalado, descomentar las líneas siguientes para instalarlo
#if (!require("compareGroups")){
#   install.packages("compareGroups")
#}
library(compareGroups)
library(tidyverse)

# Carga del conjunto de datos 'regicor' y visualización de la ayuda relacionada con este conjunto de datos
?regicor
data(regicor)

# Exploración de las dimensiones del conjunto de datos 'regicor'
dim(regicor)

# Observación de la estructura del conjunto de datos 'regicor' para entender las variables y tipos de datos
glimpse(regicor)

# Generación de un resumen estadístico del conjunto de datos 'regicor'
summary(regicor)

# Creación de una tabla descriptiva para todas las variables del conjunto de datos 'regicor', excluyendo la variable 'id'
tabla_decriptiva <- descrTable(~.-id, data = regicor)

# Visualización de la tabla descriptiva generada
tabla_decriptiva

# Exportación de la tabla descriptiva a un archivo de Word
export2word(tabla_decriptiva, file = "tabla_descriptiva.docx")
# Exportación de la misma tabla a un archivo HTML
export2html(tabla_decriptiva, file = "tabla_descriptiva.html")

# Creación de una tabla descriptiva enfocada en la variable 'sex', excluyendo la variable 'id'
tabla_sexo <- descrTable(sex ~.-id, data = regicor)

# Visualización de la tabla descriptiva por sexo
tabla_sexo

# Exportación de la tabla descriptiva por sexo a un archivo de Word
export2word(tabla_sexo, file = "tabla2.docx")

# Exportación de la tabla descriptiva por sexo a un archivo Markdown, con personalización del formato
export2md(tabla_sexo, strip = TRUE, first.strip = TRUE,
          header.background = "blue", header.color = "white",
          caption = "Estratificación de los datos regicor por sexo")

# Creación de una tabla estratificada por la historia de hipertensión utilizando la tabla descriptiva por sexo
tabla_est <- strataTable(tabla_sexo, strata = "histhtn")

# Visualización de la tabla estratificada
tabla_est

# Exportación de la tabla estratificada a un archivo Markdown, con personalización del formato
export2md(tabla_est, strip = TRUE, first.strip = TRUE,
          header.background = "blue", header.color = "white",
          caption = "Estratificación por historia de hipertensión")
