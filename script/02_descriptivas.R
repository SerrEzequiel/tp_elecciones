# ==============================================================================
# 02_descriptivas.R
# Estadísticas descriptivas de la base provincial antes de la limpieza:
# tendencia central, dispersión y distribución de cada espacio en 2019 y 2023.
#
# Lee:     input/provincias_espacios.csv, input/provincias_totales.csv
# Guarda:  output/tablas/02_descriptivas_espacios.csv
#          output/tablas/02_descriptivas_totales.csv
#          output/graficos/02_distribucion_espacios.jpg
# ==============================================================================

library(tidyverse)
library(scales)

source("utils/tema_tp.R")
source("utils/guardar_grafico.R")

provincias_espacios <- read_csv("input/provincias_espacios.csv", show_col_types = FALSE)
provincias_totales  <- read_csv("input/provincias_totales.csv",  show_col_types = FALSE)

orden_espacios <- c("Peronismo", "JxC", "Derecha liberal", "Tercera vía", "Izquierda")

# ------------------------------------------------------------------------------
# 1. Tabla: % de cada espacio por año (24 provincias)
# ------------------------------------------------------------------------------

descriptivas_espacios <- provincias_espacios |>
  mutate(espacio = factor(espacio, levels = orden_espacios)) |>
  group_by(espacio, anio) |>
  summarise(
    n        = n(),
    media    = mean(pct),
    mediana  = median(pct),
    desvio   = sd(pct),
    minimo   = min(pct),
    q1       = quantile(pct, 0.25),
    q3       = quantile(pct, 0.75),
    maximo   = max(pct),
    .groups  = "drop"
  ) |>
  mutate(across(media:maximo, ~ round(.x, 1)))

descriptivas_espacios

# ------------------------------------------------------------------------------
# 2. Tabla: participación, blancos y nulos por año
# ------------------------------------------------------------------------------

descriptivas_totales <- provincias_totales |>
  pivot_longer(c(participacion, pct_blancos, pct_nulos),
               names_to = "variable", values_to = "valor") |>
  group_by(variable, anio) |>
  summarise(
    media   = mean(valor), mediana = median(valor), desvio = sd(valor),
    minimo  = min(valor),  maximo  = max(valor),
    .groups = "drop"
  ) |>
  mutate(across(media:maximo, ~ round(.x, 1)))

descriptivas_totales

# ------------------------------------------------------------------------------
# 3. Gráfico: distribución de cada espacio entre provincias, 2019 vs 2023
# ------------------------------------------------------------------------------

datos_grafico <- provincias_espacios |>
  mutate(espacio = factor(espacio, levels = rev(orden_espacios)),
         anio    = factor(anio))

g_distribucion <- ggplot(datos_grafico, aes(x = pct, y = espacio, colour = anio)) +
  geom_boxplot(aes(fill = anio), alpha = 0.15, outlier.shape = NA, width = 0.6,
               position = position_dodge(width = 0.7)) +
  geom_point(position = position_jitterdodge(jitter.height = 0.08, dodge.width = 0.7,
                                             seed = 1),
             size = 1.4, alpha = 0.7) +
  scale_colour_manual(values = c("2019" = "#7A7A7A", "2023" = "#0072B2"), name = "Elección") +
  scale_fill_manual(values = c("2019" = "#7A7A7A", "2023" = "#0072B2"), name = "Elección") +
  scale_x_continuous(labels = label_percent(scale = 1), limits = c(0, 80)) +
  labs(
    title    = "Distribución del voto de cada espacio entre las 24 provincias",
    subtitle = "% de votos positivos por provincia. Cada punto es una provincia; la caja va del primer al tercer cuartil.",
    x        = "% de votos positivos",
    y        = NULL,
    caption  = fuente_tp
  ) +
  tema_tp()

# ------------------------------------------------------------------------------
# 4. Guardar
# ------------------------------------------------------------------------------

write_csv(descriptivas_espacios, "output/tablas/02_descriptivas_espacios.csv")
write_csv(descriptivas_totales,  "output/tablas/02_descriptivas_totales.csv")
guardar_grafico(g_distribucion, "02_distribucion_espacios.jpg", alto = 13)
