# Elimina todos los objetos del entorno de trabajo
rm(list=ls())

# Comprueba si el paquete 'BiocManager' está instalado, y si no, lo instala
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

# Instala la versión específica de Bioconductor
BiocManager::install(version = "3.19")



# Carga la librería 'metagear'
library(metagear)

# Instala el paquete 'EBImage' desde Bioconductor
BiocManager::install("EBImage")

# Vuelve a instalar el paquete 'metagear' desde CRAN
install.packages("metagear")

# Lee el archivo CSV de referencias descargado de Scopus
referencias <- read_csv("D:/Descargas/scopus (4).csv")

# Muestra un resumen de la estructura del conjunto de datos
glimpse(referencias)

# Convierte todos los nombres de las columnas a mayúsculas
names(referencias) <- toupper(names(referencias))

# Muestra los nombres de las columnas del conjunto de datos
names(referencias)

# Inicializa la distribución de esfuerzo de revisión con las referencias
REF <- effort_initialize(referencias)

# Define el grupo de revisores
grupo <- c("María", "Juan", "Pedro")

# Distribuye el esfuerzo de revisión entre los revisores definidos
ref_dist <- effort_distribute(REF, reviewers = grupo, effort = c(35, 60, 5))

# Muestra un resumen de la estructura del conjunto de datos distribuido
glimpse(ref_dist)

# Filtra las referencias asignadas al revisor "Pedro"
ref_pedro <- ref_dist |> filter(REVIEWERS == "Pedro")

# Muestra las dimensiones del conjunto de datos filtrado para Pedro
dim(ref_pedro)

# Inicializa la distribución de esfuerzo para Pedro y guarda la división
effort_distribute(ref_pedro, initialize = TRUE, reviewers = "Pedro", save_split = TRUE)

# Genera una interfaz de revisión de resúmenes para Pedro
abstract_screener("effort_Pedro.csv", aReviewer = "Pedro")

# Lee el archivo CSV con las revisiones de Pedro
revision_pedro <- read_csv("effort_Pedro.csv")

# Muestra un resumen de la estructura del conjunto de datos de revisiones
glimpse(revision_pedro)

# Filtra y muestra las revisiones donde se incluyeron los estudios
revision_pedro |> filter(INCLUDE == "YES") |> glimpse()

# Define las etapas del diagrama PRISMA
etapas <- c(
  "START_PHASE: Número de estudios identificados mediante búsqueda en bases de datos",
  "START_PHASE: Número de estudios adicionales identificados mediante otras fuentes",
  "Número de estudios después de eliminar duplicados",
  "Número de estudios con título y resumen revisados",
  "EXCLUDE_PHASE: Número de estudios excluidos",
  "Número de artículos de texto completo evaluados para determinar su elegibilidad",
  "EXCLUDE_PHASE: Número de artículos de texto completo excluidos por no cumplir con los criterios de elegibilidad",
  "Número de estudios incluidos en la síntesis cualitativa",
  "EXCLUDE_PHASE: Número de estudios excluidos por reportar datos incompletos",
  "Número final de estudios incluidos en la síntesis cuantitativa (meta-análisis)"
)

# Genera el diagrama PRISMA estándar
plot1 <- plot_PRISMA(etapas)

# Genera el diagrama PRISMA con diseño vintage
plot2 <- plot_PRISMA(etapas, design = "vintage")
