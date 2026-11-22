# ==============================================================================
# 01_limpieza_base_provincial.R
# Arma la base provincial 2019-2023: % de cada uno de los 5 espacios sobre
# votos positivos, y los totales de cada provincia (electores, blancos, nulos).
#
# Lee:     raw/presi_1983_2019.csv, raw/presi_2023.csv
#          auxiliar/espacios_2019_2023.csv   (qué espacio es cada lista)
# Guarda:  input/provincias_espacios.csv     (24 provincias x 2 años x 5 espacios)
#          input/provincias_totales.csv      (24 provincias x 2 años)
# ==============================================================================

library(tidyverse)

source("utils/limpiar_nombre_lista.R")

# ------------------------------------------------------------------------------
# 1. Leer y unir 2019 y 2023
# ------------------------------------------------------------------------------

# La base 1983-2019 trae todas las elecciones; el trabajo usa solo 2019 y 2023
# (devolución de la entrega 1: centrarse en una o dos elecciones)
resultados <- bind_rows(
  read_csv("raw/presi_1983_2019.csv", show_col_types = FALSE),
  read_csv("raw/presi_2023.csv",      show_col_types = FALSE)
) |>
  filter(anio %in% c(2019, 2023)) |>
  mutate(lista = limpiar_nombre_lista(lista))

espacios_aux <- read_csv("auxiliar/espacios_2019_2023.csv", show_col_types = FALSE)

# ------------------------------------------------------------------------------
# 2. Totales por provincia (participación, blancos y nulos)
# ------------------------------------------------------------------------------

provincias_totales <- resultados |>
  group_by(anio, provincia) |>
  summarise(
    electores       = first(electores),
    votos_positivos = sum(votos[tipo_voto == "positivo"]),
    votos_blancos   = sum(votos[tipo_voto == "blanco"]),
    votos_nulos     = sum(votos[tipo_voto == "nulo"]),
    .groups = "drop"
  ) |>
  mutate(
    votos_emitidos = votos_positivos + votos_blancos + votos_nulos,
    participacion  = votos_emitidos / electores * 100,
    pct_blancos    = votos_blancos / votos_emitidos * 100,
    pct_nulos      = votos_nulos   / votos_emitidos * 100
  )

# ------------------------------------------------------------------------------
# 3. % de cada espacio sobre votos positivos
# ------------------------------------------------------------------------------

positivos <- resultados |>
  filter(tipo_voto == "positivo") |>
  left_join(espacios_aux, by = c("anio", "lista"))

# Control: ninguna lista puede quedar sin espacio asignado
sin_espacio <- positivos |> filter(is.na(espacio)) |> distinct(anio, lista)
if (nrow(sin_espacio) > 0) {
  print(sin_espacio)
  stop("Hay listas sin espacio en auxiliar/espacios_2019_2023.csv")
}

provincias_espacios <- positivos |>
  group_by(anio, provincia, espacio) |>
  summarise(votos = sum(votos), .groups = "drop") |>
  group_by(anio, provincia) |>
  mutate(pct = votos / sum(votos) * 100) |>
  ungroup()

# Controles: 24 provincias x 2 años x 5 espacios y cada provincia suma 100
stopifnot(
  nrow(provincias_espacios) == 24 * 2 * 5,
  all(abs(tapply(provincias_espacios$pct,
                 paste(provincias_espacios$anio, provincias_espacios$provincia),
                 sum) - 100) < 1e-6)
)

# ------------------------------------------------------------------------------
# 4. Guardar
# ------------------------------------------------------------------------------

write_csv(provincias_espacios, "input/provincias_espacios.csv")
write_csv(provincias_totales,  "input/provincias_totales.csv")
