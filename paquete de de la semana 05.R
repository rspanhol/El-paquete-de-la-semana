# Eliminar todos los objetos en el entorno de trabajo
rm(list = ls())

# Cargar las bibliotecas necesarias
library(tidyverse)
library(tidytext)

# Definir la ruta del archivo de texto en GitHub
path = "https://raw.githubusercontent.com/rspanhol/El-paquete-de-la-semana/main/cien%20años%20de%20soledad.txt"

# Leer el contenido del archivo de texto línea por línea
novela <- scan(file = path, what = "character", sep = "\n")

# Verificar la clase del objeto 'novela'
class(novela)

# Mostrar las primeras 5 líneas de la novela
novela[1:5]

# Mostrar la línea 11814 de la novela
novela[11814]

# Mostrar la última línea de la novela
novela[length(novela)]

# Eliminar las primeras 5 líneas del texto (posiblemente metadatos o encabezados)
novela <- novela[6:length(novela)]

# Mostrar la primera línea del texto procesado
novela[1]

# Convertir la novela en un tibble con una columna llamada 'word'
novela_df <- tibble(word = novela)

# Mostrar el tibble
novela_df

# Descomponer el texto en unigramas (palabras individuales), sin convertir a minúsculas
novela_unigrams <- 
  novela_df |> 
  unnest_tokens(word, word, to_lower = F)

# Mostrar los unigramas
novela_unigrams

# Crear una lista de capítulos en números romanos del I al XX
capitulos <- as.roman(1:20)
capitulos |> class()

# Convertir los números romanos a caracteres
capitulos <- as.character(capitulos)

# Encontrar las posiciones de los capítulos en el texto
which(novela_unigrams$word %in% capitulos)

# Crear un tibble con las posiciones de los capítulos
pos <- tibble(pos_cap = which(novela_unigrams$word %in% capitulos))

# Mostrar el tibble de posiciones
pos

# Mostrar el unigrama en la posición 5868
novela_unigrams[5868,]

# Añadir una columna con la posición anterior a cada capítulo
pos <- pos |> mutate(pos_ant = pos_cap - 1)

# Mostrar el tibble de posiciones actualizado
pos
pos
novela_unigrams[5867,]

# Extraer los nombres de los capítulos del texto
nombre_cap <- novela_unigrams[pos$pos_ant,] |> pull()

# Mostrar los nombres de los capítulos
nombre_cap

# Filtrar las posiciones para excluir cualquier entrada no deseada (por ejemplo, 'siglo')
pos_filtrada <- 
  pos |> 
  mutate(nombre_cap = nombre_cap) |> 
  filter(nombre_cap != "siglo")
pos_filtrada

# Calcular las diferencias entre las posiciones de los capítulos
posic <- diff(pos_filtrada$pos_ant)

# Calcular el número de líneas desde el último capítulo hasta el final del texto
ultima <- nrow(novela_unigrams) - pos_filtrada[20, 1]
ultima

# Crear un vector con el número de líneas de cada capítulo, incluyendo el último capítulo
pos_plus_final <- c(posic, ultima)
pos_plus_final

# Crear un vector que repite el número del capítulo según la longitud de cada capítulo
mi_vector <- mapply(rep, 1:20, pos_plus_final)

# Aplanar el vector
mi_vector <- unlist(mi_vector)
mi_vector

# Asignar el número de capítulo a cada unigrama
novela_unigrams$capitulo <- c(mi_vector, 20, 20)
novela_unigrams

# Mostrar las últimas filas del tibble
novela_unigrams |> tail()

# Convertir la columna 'capitulo' a factor
novela_unigrams$capitulo <- factor(novela_unigrams$capitulo)

# Mostrar la estructura del tibble
novela_unigrams |> glimpse()

# Mostrar los niveles del factor 'capitulo'
novela_unigrams$capitulo |> levels()

# Contar la frecuencia de las palabras en cada capítulo y filtrar las que aparecen al menos 100 veces
# Graficar los resultados
novela_unigrams |> 
  group_by(capitulo, word) |> 
  count(word, sort = T) |> 
  filter(n >= 100) |> 
  ggplot(aes(x = reorder(word, n), y = n, fill = capitulo)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~capitulo, scales = "free")

# Cargar la lista de stopwords en español
load("spanishstp.rds")

# Mostrar las stopwords en español
spanish_stop_words

# Limpiar los unigramas eliminando las stopwords
novela_unigrams_clean <- novela_unigrams |> 
  mutate(word = tolower(word)) |> 
  anti_join(spanish_stop_words)

# Mostrar los unigramas limpios
novela_unigrams_clean

# Contar la frecuencia de las palabras en cada capítulo y filtrar las que aparecen al menos 10 veces
# Graficar los resultados
novela_unigrams_clean |> 
  group_by(capitulo, word) |> 
  count(word, sort = T) |> 
  filter(n >= 10) |> 
  ggplot(aes(x = reorder(word, n), y = n, fill = capitulo)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~capitulo, scales = "free")

# Descomponer el texto en bigramas (pares de palabras)
novela_bigrams <- 
  novela_df |> 
  unnest_tokens(output = bigram, token = "ngrams", n = 2, to_lower = F)

# Mostrar los bigramas
novela_bigrams

# Crear etiquetas para los capítulos
etiquetas <- paste("Capítulo", as.roman(1:20))

# Verificar la clase de las etiquetas
etiquetas |> class()

# Encontrar las posiciones de los bigramas de los capítulos en el texto
limites <- which(novela_bigrams$bigram %in% etiquetas)

# Añadir un valor de límite infinito para el último capítulo
limites <- c(limites, Inf)

# Mostrar el primer bigrama
novela_bigrams[1,]

# Mostrar el bigrama en la posición 120243
novela_bigrams[120243,]

# Asignar el número de capítulo a cada bigrama
novela_bigrams$capitulo <- cut(
  1:nrow(novela_bigrams),
  breaks = limites,
  label = etiquetas,
  right = F
)

# Mostrar los bigramas con el número de capítulo asignado
novela_bigrams  

# Separar los bigramas en dos columnas: 'word1' y 'word2'
novela_bigrams_sep <- 
  novela_bigrams |> 
  separate(bigram, c("word1", "word2"), sep = " ")

# Mostrar los bigramas separados
novela_bigrams_sep

# Filtrar los bigramas eliminando las stopwords
novela_bigrams_filtrada <- 
  novela_bigrams_sep |> 
  filter(!word1 %in% spanish_stop_words$word) |> 
  filter(!word2 %in% spanish_stop_words$word)

# Contar la frecuencia de los bigramas filtrados
novela_bigrams_filtrada |> 
  count(word1, word2, sort = T)

# Unir las palabras de los bigramas nuevamente en una sola columna
novela_unida <- novela_bigrams_filtrada |> 
  unite(bigram, word1, word2, sep = " ")

# Contar la frecuencia de los bigramas por capítulo y filtrar los que aparecen al menos 5 veces
# Graficar los resultados
novela_unida |> 
  count(capitulo, bigram, sort = T) |> 
  filter(n >= 5) |> 
  ggplot(aes(x = reorder(bigram, n), y = n, fill = capitulo)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~capitulo, scales = "free")


