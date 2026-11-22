# ==============================================================================
# 00a_descarga_1983_2019.R  (OPCIONAL: el resultado ya está en raw/)
# Descargar resultados presidenciales por provincia, 1983-2019
#
# Fuente: escrutinios definitivos de la Dirección Nacional Electoral (DINE),
#         compilados por el proyecto electorAr (PoliticaArgentina en GitHub)
#         https://github.com/PoliticaArgentina/data_warehouse
#
# Resultado: raw/presi_1983_2019.csv
# Solo hace falta correrlo UNA vez (necesita internet).
# ==============================================================================

library(tidyverse)

# ------------------------------------------------------------------------------
# 1. Parámetros
# ------------------------------------------------------------------------------

# Carpeta donde están los archivos (uno por provincia y por año)
url_base <- "https://raw.githubusercontent.com/PoliticaArgentina/data_warehouse/master/electorAr/data/escrutinios_definitivos/"

# Cada archivo se llama <codigo>_presi_gral<año>.csv
# Esta tabla traduce el código del archivo al nombre de la provincia
provincias <- tribble(
  ~codigo,      ~provincia,
  "caba",       "CABA",
  "pba",        "Buenos Aires",
  "catamarca",  "Catamarca",
  "chaco",      "Chaco",
  "chubut",     "Chubut",
  "cordoba",    "Córdoba",
  "corrientes", "Corrientes",
  "erios",      "Entre Ríos",
  "formosa",    "Formosa",
  "jujuy",      "Jujuy",
  "pampa",      "La Pampa",
  "rioja",      "La Rioja",
  "mendoza",    "Mendoza",
  "misiones",   "Misiones",
  "neuquen",    "Neuquén",
  "rnegro",     "Río Negro",
  "salta",      "Salta",
  "sjuan",      "San Juan",
  "sanluis",    "San Luis",
  "scruz",      "Santa Cruz",
  "sfe",        "Santa Fe",
  "santiago",   "Santiago del Estero",
  "tdf",        "Tierra del Fuego",
  "tucuman",    "Tucumán"
)

anios <- c(1983, 1989, 1995, 1999, 2003, 2007, 2011, 2015, 2019)

# ------------------------------------------------------------------------------
# 2. Función que descarga UNA elección (una provincia en un año)
# ------------------------------------------------------------------------------

bajar_eleccion <- function(codigo, anio) {
  url <- paste0(url_base, codigo, "_presi_gral", anio, ".csv")

  read_csv(url,
           col_types = cols(lista = col_character(), .default = col_double())) |>
    mutate(codigo = codigo, anio = anio)
}

# Probamos con un caso antes de bajar todo
bajar_eleccion("formosa", 2019)

# ------------------------------------------------------------------------------
# 3. Descargar las 216 combinaciones provincia x año (tarda unos minutos)
# ------------------------------------------------------------------------------

combinaciones <- expand_grid(codigo = provincias$codigo, anio = anios)

# map2() aplica la función a cada par (codigo, anio) y devuelve una lista
# de tablas; bind_rows() las apila en una sola
presi_raw <- map2(combinaciones$codigo, combinaciones$anio, bajar_eleccion) |>
  bind_rows()

# ------------------------------------------------------------------------------
# 4. Agregar nombre de provincia y clasificar el tipo de voto
# ------------------------------------------------------------------------------

presi_1983_2019 <- presi_raw |>
  left_join(provincias, by = "codigo") |>
  mutate(
    tipo_voto = case_when(
      str_detect(lista, regex("blanco", ignore_case = TRUE)) ~ "blanco",
      str_detect(lista, regex("nulo",   ignore_case = TRUE)) ~ "nulo",
      TRUE                                                   ~ "positivo"
    )
  ) |>
  select(anio, provincia, lista, tipo_voto, votos, electores)

# Control: tienen que aparecer las 24 provincias en los 9 años
presi_1983_2019 |>
  distinct(anio, provincia) |>
  count(anio)

# ------------------------------------------------------------------------------
# 5. Guardar
# ------------------------------------------------------------------------------

write_csv(presi_1983_2019, "raw/presi_1983_2019.csv")
