# Elimina todos los objetos del entorno de trabajo para comenzar con un entorno limpio
rm(list = ls())

# Instala los paquetes necesarios para el análisis (descomentar estas líneas si los paquetes no están instalados)
# install.packages("ollamar")
# install.packages("mall")
# install.packages("tictoc")

# Carga las bibliotecas requeridas para el análisis y el manejo del tiempo de ejecución
library(tidyverse)  # Paquete para manipulación de datos
library(tictoc)     # Paquete para medir el tiempo de ejecución del código

# Carga las bibliotecas específicas para la interacción con modelos de lenguaje y análisis de datos
library(ollamar)    # Paquete para la interacción con LLMs utilizando Ollama
library(mall)       # Paquete para ejecutar múltiples predicciones de LLMs sobre marcos de datos

# Mide el tiempo de ejecución para descargar el modelo de lenguaje "llama3.1"
tic()
pull("llama3.1")  # Descarga el modelo de lenguaje "llama3.1" desde Ollama
toc()

# Prueba la conexión con Ollama para asegurarse de que el sistema está configurado correctamente
test_connection()

# Lista los modelos de lenguaje disponibles para su uso
list_models()

# Mide el tiempo de ejecución para generar texto usando el modelo de lenguaje "llama3.1"
tic()
txt <- generate("llama3.1",  # Utiliza el modelo "llama3.1"
                "relata en 20 palabras el origen de la tierra",  # Prompt para generar el texto
                output = "text")  # Especifica que el resultado debe ser en formato de texto
toc()

# Imprime el texto generado por el modelo
txt

# Carga el conjunto de datos de ejemplo "reviews" que contiene reseñas de texto
data("reviews")

# Muestra las dimensiones del conjunto de datos "reviews"
reviews |> dim()

# Realiza un análisis de sentimientos sobre las reseñas en el conjunto de datos "reviews"
reviews |> 
  llm_sentiment(review)

# Mide el tiempo de ejecución para traducir las reseñas al español
tic()
reviews |> 
  llm_translate(review, "spanish")  # Traduce el texto de la columna "review" al español
toc()

# Lee un archivo CSV llamado "farmacos.csv" que contiene datos de fármacos
datos <- read_csv("farmacos.csv")

# Mide el tiempo de ejecución para traducir los documentos en la columna "documents" al español
tic()
datos <- datos |> 
  llm_translate(documents, "spanish")  # Traduce la columna "documents" al español
toc()

# Selecciona y muestra la columna de traducciones generadas
datos |> select(.translation)

# Mide el tiempo de ejecución para extraer categorías de medicamentos de la columna "drug_name"
tic()
datos <- datos |> 
  llm_extract(drug_name, labels = "drug category")  # Extrae la categoría de medicamentos
toc()

