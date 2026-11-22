# ==============================================================================
# 03_faltantes_outliers.R
# Diagnóstico de faltantes y outliers en la base provincial, decisión tomada y
# descriptivas antes y después (con y sin las provincias atípicas).
#
# Lee:     raw/presi_1983_2019.csv, raw/presi_2023.csv
#          input/provincias_espacios.csv   (script 01)
# Guarda:  output/tablas/03_faltantes.csv
#          output/tablas/03_outliers.csv
#          output/tablas/03_descriptivas_con_sin_outliers.csv
#          output/graficos/03_outliers_zscore.jpg
#          input/provincias_matriz.csv     (24 provincias x 10 variables, para el clustering)
#          input/provincias_atipicas.csv   (provincias con algún |z| > 2,5)
# ==============================================================================

library(tidyverse)
library(scales)
library(ggrepel)

source("utils/tema_tp.R")
source("utils/fuente_tp.R")
source("utils/guardar_grafico.R")
source("utils/nombre_variable.R")
source("utils/etiqueta_variable.R")

umbral_z     <- 2.5   # mismo criterio que en la entrega 2
umbral_texto <- format(umbral_z, decimal.mark = ",")

orden_espacios <- c("Peronismo", "JxC", "Derecha liberal", "Tercera vía", "Izquierda")

# ------------------------------------------------------------------------------
# 1. Faltantes: cuántos NA tiene cada variable de la base cruda (2019 y 2023)
# ------------------------------------------------------------------------------

crudo <- bind_rows(
  read_csv("raw/presi_1983_2019.csv", show_col_types = FALSE),
  read_csv("raw/presi_2023.csv",      show_col_types = FALSE)
) |>
  filter(anio %in% c(2019, 2023))

faltantes <- crudo |>
  group_by(anio) |>
  summarise(
    filas = n(),
    across(c(provincia, lista, tipo_voto, votos, electores), ~ sum(is.na(.x)),
           .names = "na_{.col}"),
    provincias = n_distinct(provincia),
    .groups = "drop"
  )

faltantes   # 0 NA en todas las variables y 24 provincias en cada año

provincias_espacios <- read_csv("input/provincias_espacios.csv", show_col_types = FALSE)

# Control: cada provincia tiene los 5 espacios en los dos años (sin celdas vacías)
stopifnot(nrow(provincias_espacios) == 240, !anyNA(provincias_espacios$pct))

# ------------------------------------------------------------------------------
# 2. Matriz para el clustering: una fila por provincia, 10 variables
# ------------------------------------------------------------------------------

# Orden de las columnas: por espacio (de mayor a menor peso) y dentro de cada
# espacio, 2019 y 2023
orden_variables <- nombre_variable(rep(orden_espacios, each = 2), rep(c(2019, 2023), 5))

matriz <- provincias_espacios |>
  mutate(variable = nombre_variable(espacio, anio)) |>
  select(provincia, variable, pct) |>
  pivot_wider(names_from = variable, values_from = pct) |>
  select(provincia, all_of(orden_variables))

# ------------------------------------------------------------------------------
# 3. Outliers: z-score de cada provincia en cada variable
# ------------------------------------------------------------------------------

# Con 24 casos el rango intercuartílico marca muy pocos valores; el z-score
# compara cada provincia con el promedio de las 24 en esa variable. El IQR se
# calcula igual, como comparación.
zscores <- matriz |>
  pivot_longer(-provincia, names_to = "variable", values_to = "pct") |>
  group_by(variable) |>
  mutate(
    z         = (pct - mean(pct)) / sd(pct),
    q1        = quantile(pct, 0.25),
    q3        = quantile(pct, 0.75),
    fuera_iqr = pct < q1 - 1.5 * (q3 - q1) | pct > q3 + 1.5 * (q3 - q1)
  ) |>
  ungroup() |>
  mutate(atipico = abs(z) > umbral_z)

outliers <- zscores |>
  filter(atipico) |>
  mutate(etiqueta = etiqueta_variable(variable)) |>
  select(provincia, variable, etiqueta, pct, z, fuera_iqr) |>
  arrange(desc(abs(z))) |>
  mutate(across(c(pct, z), ~ round(.x, 2)))

outliers

provincias_atipicas <- distinct(outliers, provincia)

# ------------------------------------------------------------------------------
# 4. Decisión: se mantienen. Descriptivas con y sin las provincias atípicas
# ------------------------------------------------------------------------------

# Los valores extremos son genuinos (por ejemplo, Tercera vía 2023 en Córdoba:
# Schiaretti era el gobernador). No son errores de carga, así que se mantienen.
# Para ver cuánto pesan, se comparan las descriptivas con y sin esas provincias.

resumir <- function(datos, version) {
  datos |>
    group_by(variable) |>
    summarise(media = mean(pct), mediana = median(pct), desvio = sd(pct),
              .groups = "drop") |>
    mutate(version = version)
}

comparacion <- bind_rows(
  resumir(zscores, "con_todas"),
  resumir(filter(zscores, !provincia %in% provincias_atipicas$provincia), "sin_atipicas")
) |>
  pivot_wider(names_from = version, values_from = c(media, mediana, desvio)) |>
  mutate(
    dif_media   = media_sin_atipicas - media_con_todas,
    dif_mediana = mediana_sin_atipicas - mediana_con_todas,
    across(where(is.numeric), ~ round(.x, 2)),
    variable    = factor(variable, levels = orden_variables)
  ) |>
  arrange(variable)

comparacion

# ------------------------------------------------------------------------------
# 5. Gráfico: z-score de cada provincia en cada variable
# ------------------------------------------------------------------------------

datos_grafico <- zscores |>
  mutate(etiqueta = factor(etiqueta_variable(variable),
                           levels = rev(etiqueta_variable(orden_variables))))

g_outliers <- ggplot(datos_grafico, aes(x = z, y = etiqueta)) +
  annotate("rect", xmin = -umbral_z, xmax = umbral_z, ymin = -Inf, ymax = Inf,
           fill = "grey95") +
  geom_vline(xintercept = c(-umbral_z, umbral_z), linetype = "dashed",
             colour = "grey50", linewidth = 0.4) +
  geom_point(aes(colour = atipico), size = 1.8, alpha = 0.8) +
  geom_text_repel(data = filter(datos_grafico, atipico),
                  aes(label = provincia), size = 2.8, colour = "#B9470B",
                  min.segment.length = 0, seed = 1) +
  scale_colour_manual(values = c("FALSE" = "grey55", "TRUE" = "#D55E00"),
                      labels = c("Dentro del rango",
                                 paste0("Atípico (|z| > ", umbral_texto, ")")),
                      name = NULL) +
  scale_x_continuous(labels = label_number(decimal.mark = ",")) +
  labs(
    title    = paste0(nrow(provincias_atipicas),
                      " provincias tienen algún valor atípico en las variables del clustering"),
    subtitle = paste0("z-score de cada provincia en cada variable (% de votos positivos). ",
                      "Zona gris: |z| ≤ ", umbral_texto, ". Se mantienen por ser valores genuinos."),
    x        = "z-score (desvíos respecto del promedio de las 24 provincias)",
    y        = NULL,
    caption  = fuente_tp()
  ) +
  tema_tp()

# ------------------------------------------------------------------------------
# 6. Guardar
# ------------------------------------------------------------------------------

write_csv(faltantes,   "output/tablas/03_faltantes.csv")
write_csv(outliers,    "output/tablas/03_outliers.csv")
write_csv(comparacion, "output/tablas/03_descriptivas_con_sin_outliers.csv")
guardar_grafico(g_outliers, "03_outliers_zscore.jpg", alto = 15)

# Se mantienen todos los casos: la matriz del clustering es la base completa
write_csv(matriz,              "input/provincias_matriz.csv")
write_csv(provincias_atipicas, "input/provincias_atipicas.csv")
