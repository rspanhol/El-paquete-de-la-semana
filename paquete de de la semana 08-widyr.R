# Limpiar el ambiente de trabajo eliminando todos los objetos
rm(list = ls())

# Cargar las librerías necesarias
library(tidyverse)
library(gapminder)
library(widyr)

# Mostrar la documentación del conjunto de datos 'gapminder'
?gapminder

# Calcular la media de la esperanza de vida por año y graficar los resultados
gapminder |> 
  group_by(year) |> 
  summarise(media_expLife = mean(lifeExp)) |> 
  ggplot(aes(x = year, y = media_expLife, size = media_expLife)) +
  geom_point() +
  geom_smooth()

# Calcular la correlación por pares entre los países en función de los años y la esperanza de vida
gapminder |> 
  pairwise_cor(country, year, lifeExp, sort = TRUE)

# Calcular la distancia por pares entre los países en función de los años y la esperanza de vida, y ordenar por distancia
gapminder |> 
  pairwise_dist(country, year, lifeExp, sort = TRUE) |> 
  arrange(distance)

# Cargar la librería 'unvotes' y convertir los votos a numéricos
library(unvotes)
un_votes$vote <- as.numeric(un_votes$vote)

# Calcular la correlación por pares de los votos de los países con respecto a los Estados Unidos y graficar los 30 países con mayor correlación
un_votes |> 
  pairwise_cor(country, rcid, vote, sort = TRUE) |> 
  filter(item1 == "United States") |> 
  top_n(30, abs(correlation)) |> 
  ggplot(aes(correlation, reorder(item2, correlation))) +
  geom_col()

# Cargar los datos de bigramas de una novela y las palabras vacías en español
load("novela_bigrams.rds")
load("spanishstp.rds")
novela_bigrams

# Separar los bigramas en dos palabras y filtrar aquellas que no son palabras vacías
novela_sep <- novela_bigrams |> 
  separate(bigram, into = c("word1", "word2"), sep = " ") |> 
  filter(!word1 %in% spanish_stop_words$word,
         !word2 %in% spanish_stop_words$word)

# Unir las palabras en bigramas nuevamente
novela_unida <- novela_sep |> 
  unite(bigram, c(word1, word2), sep = " ")

# Contar la frecuencia de cada bigrama y calcular la correlación por pares de los bigramas con respecto al capítulo para "Mauricio Babilonia"
novela_unida |> 
  add_count(bigram, name = "frec_word") |> 
  filter(frec_word >= 20) |> 
  pairwise_cor(bigram, capitulo, value = frec_word, sort = TRUE) |> 
  filter(item1 == "Mauricio Babilonia")

# Calcular el PMI (Pointwise Mutual Information) por pares de los bigramas con respecto al capítulo para "Mauricio Babilonia"
novela_unida |> 
  add_count(bigram, name = "frec_word") |> 
  filter(frec_word >= 20) |> 
  pairwise_pmi(bigram, capitulo, sort = TRUE) |> 
  filter(item1 == "Mauricio Babilonia")














