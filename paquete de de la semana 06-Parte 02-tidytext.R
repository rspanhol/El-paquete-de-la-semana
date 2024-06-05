# Limpieza del entorno de trabajo
rm(list = ls())

# Carga de bibliotecas
library(tidyverse)
library(tidytext)

# Carga del archivo de datos
load("novela_unigrams.rds")

# Conteo de palabras por capítulo
palabras <- novela_unigrams |>
  group_by(capitulo, word) |>
  count(word, sort = T)

# Cálculo del total de palabras por capítulo
palabras_totales <- palabras |>
  group_by(capitulo) |>
  summarise(total = sum(n))

# Combinación de datos de frecuencia de palabras con totales
palabras <- left_join(palabras, palabras_totales)

# Visualización de la distribución de palabras
palabras |>
  ggplot(aes(x = n / total, fill = capitulo)) +
  geom_histogram(show.legend = F) +
  xlim(NA, 0.003) +
  facet_wrap(~capitulo, scales = "free")

# Calculo de la frecuencia relativa y el ranking de palabras
palabras_rank <- palabras |>
  group_by(capitulo) |>
  mutate(rank = row_number(), frecuencia = n / total) |>
  ungroup()

# Visualización de la frecuencia relativa en escala logarítmica
palabras_rank |>
  ggplot(aes(x = rank, y = frecuencia, color = capitulo)) +
  geom_line(show.legend = F) +
  scale_x_log10() +
  scale_y_log10()

# Cálculo del TF-IDF
novela_tf_idf <- palabras |>
  bind_tf_idf(word, capitulo, n)

# Ordenamiento y visualización del TF-IDF
novela_tf_idf |>
  arrange(desc(tf_idf))

novela_tf_idf |>
  group_by(capitulo) |>
  slice_max(tf_idf, n = 10) |>
  ungroup() |>
  ggplot(aes(x = tf_idf, y = reorder(word, tf_idf), fill = capitulo)) +
  geom_col(show.legend = F) +
  facet_wrap(~capitulo, scales = "free")

# Carga de léxicos de sentimientos
lexico_afinn <- read_csv("https://raw.githubusercontent.com/jboscomendoza/lexicos-nrc-afinn/master/lexico_afinn.csv")
lexico_nrc <- read_csv("https://raw.githubusercontent.com/jboscomendoza/lexicos-nrc-afinn/master/lexico_nrc.csv")

# Análisis de sentimientos con léxico AFINN
novela_unigrams |>
  inner_join(lexico_afinn, by = join_by(word == palabra)) |>
  count(word, sort = T)

novela_unigrams |>
  inner_join(lexico_afinn, by = join_by(word == palabra)) |>
  mutate(num_linea = row_number()) |>
  filter(capitulo %in% c(1, 20)) |>
  ggplot(aes(x = reorder(num_linea %/% 20, puntuacion), y = puntuacion, fill = capitulo)) +
  geom_col(show.legend = F) +
  geom_hline(yintercept = 0) +
  facet_wrap(~capitulo, scales = "free") +
  theme_classic()

# Instalación y uso de la biblioteca `janitor` para análisis de sentimientos con léxico NRC
if (!require(janitor)) {
  install.packages("janitor")
}
library(janitor)

novela_unigrams |>
  inner_join(lexico_nrc, by = join_by(word == palabra)) |>
  mutate(num_linea = row_number()) |>
  count(capitulo, num_linea, sentimiento) |>
  tabyl(capitulo, sentimiento) |>
  pivot_longer(cols = -capitulo, names_to = "sentimiento") |>
  ggplot(aes(x = reorder(sentimiento, value), y = value, fill = capitulo)) +
  geom_col(show.legend = F) +
  coord_flip() +
  facet_wrap(~capitulo, scales = "free") +
  theme_classic()




