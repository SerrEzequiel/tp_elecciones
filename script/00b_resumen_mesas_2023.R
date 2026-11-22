# ==============================================================================
# 00b_resumen_mesas_2023.R  (OPCIONAL: el resultado ya está en raw/)
# Procesar resultados presidenciales 2023 (archivo de la DINE, por mesa)
#
# Fuente: escrutinio provisorio de la Dirección Nacional Electoral (DINE)
#         https://www.argentina.gob.ar/dine/resultados-electorales
#
# ANTES DE CORRER:
#   R no abre archivos .rar. Descomprimir el .rar a mano (clic derecho ->
#   "Extraer aquí" con WinRAR o 7-Zip) y poner el/los .csv que trae en la
#   carpeta  raw/2023_crudo/  del proyecto. Ese archivo pesa 1 GB y no se sube
#   al repositorio; el resumen por provincia sí.
#
# Resultado: raw/presi_2023.csv (pesa pocos KB)
# Solo hace falta correrlo UNA vez. Después se puede borrar el archivo grande.
# ==============================================================================

library(tidyverse)

# ------------------------------------------------------------------------------
# 1. Buscar los archivos descomprimidos
# ------------------------------------------------------------------------------

carpeta_crudo <- "raw/2023_crudo"

archivos <- list.files(carpeta_crudo, pattern = "\\.csv$",
                       full.names = TRUE, recursive = TRUE)

archivos                          # ¿qué archivos hay?
round(file.size(archivos) / 1e6)  # tamaño de cada uno, en MB

if (length(archivos) == 0) {
  stop("No hay archivos .csv en ", carpeta_crudo,
       ". Descomprimí el .rar y poné los .csv en esa carpeta.")
}

# Miramos los nombres de las columnas leyendo solo 5 filas (es rápido)
names(read_delim(archivos[1], delim = NULL, n_max = 5, show_col_types = FALSE))

# ------------------------------------------------------------------------------
# 2. Leer SOLO lo que necesitamos
# ------------------------------------------------------------------------------

# De las ~23 columnas del archivo usamos estas 11
columnas <- c("eleccion_tipo", "distrito_id", "distrito_nombre",
              "seccion_id", "circuito_id", "mesa_id", "mesa_electores",
              "cargo_nombre", "agrupacion_nombre", "votos_tipo",
              "votos_cantidad")

# Función que lee un archivo y se queda solo con la categoría presidente.
#   delim = NULL -> R detecta solo si el separador es "," o ";"
#   todo se lee como texto y después convertimos lo numérico
leer_presidente <- function(archivo) {
  read_delim(archivo,
             delim      = NULL,
             col_select = all_of(columnas),
             col_types  = cols(.default = col_character())) |>
    filter(str_detect(cargo_nombre, "^PRESIDENTE"))
}

# Leemos todos los archivos (si hay uno solo, lee ese). Puede tardar varios
# minutos: esperar a que desaparezca el botón rojo de la consola
presi_mesas <- map(archivos, leer_presidente) |>
  bind_rows()

gc()  # libera memoria

# Control: ¿qué tipos de elección hay? (si aparecen PASO o balotaje, los sacamos)
presi_mesas |> count(eleccion_tipo, cargo_nombre)

presi_mesas <- presi_mesas |>
  filter(eleccion_tipo == "GENERAL") |>
  mutate(
    distrito_id    = as.integer(distrito_id),
    mesa_electores = as.numeric(mesa_electores),
    votos_cantidad = as.numeric(votos_cantidad)
  )

if (nrow(presi_mesas) == 0) {
  stop("No quedaron filas. Revisar los valores de eleccion_tipo y cargo_nombre.")
}

# ------------------------------------------------------------------------------
# 3. Nombre de cada provincia
# ------------------------------------------------------------------------------

# Código de distrito electoral -> nombre (numeración oficial de la justicia
# electoral). Usamos los mismos nombres que en la base 1983-2019.
codigos_distrito <- tibble(
  distrito_id = 1:24,
  provincia = c("CABA", "Buenos Aires", "Catamarca", "Córdoba", "Corrientes",
                "Chaco", "Chubut", "Entre Ríos", "Formosa", "Jujuy",
                "La Pampa", "La Rioja", "Mendoza", "Misiones", "Neuquén",
                "Río Negro", "Salta", "San Juan", "San Luis", "Santa Cruz",
                "Santa Fe", "Santiago del Estero", "Tucumán", "Tierra del Fuego")
)

# Control: tienen que aparecer 24 filas, cada nombre al lado de su equivalente
presi_mesas |>
  distinct(distrito_id, distrito_nombre) |>
  left_join(codigos_distrito, by = "distrito_id") |>
  arrange(distrito_id) |>
  print(n = 30)

# ------------------------------------------------------------------------------
# 4. Pasar de mesas a provincias
# ------------------------------------------------------------------------------

# VOTOS: sumamos todas las mesas de cada provincia, por lista.
# Dejamos positivos, blancos y nulos para que sea comparable con 1983-2019
# (impugnados, recurridos y comando no figuran en esos años)
votos_2023 <- presi_mesas |>
  filter(votos_tipo %in% c("POSITIVO", "EN BLANCO", "NULO")) |>
  mutate(
    tipo_voto = case_when(
      votos_tipo == "POSITIVO"  ~ "positivo",
      votos_tipo == "EN BLANCO" ~ "blanco",
      votos_tipo == "NULO"      ~ "nulo"
    ),
    lista = case_when(
      tipo_voto == "positivo" ~ agrupacion_nombre,
      tipo_voto == "blanco"   ~ "Votos En Blanco",
      tipo_voto == "nulo"     ~ "Votos Nulos"
    )
  ) |>
  group_by(distrito_id, lista, tipo_voto) |>
  summarise(votos = sum(votos_cantidad, na.rm = TRUE), .groups = "drop")

# ELECTORES: cada mesa aparece repetida una vez por opción de voto,
# así que primero dejamos UNA fila por mesa y después sumamos
electores_2023 <- presi_mesas |>
  distinct(distrito_id, seccion_id, circuito_id, mesa_id, mesa_electores) |>
  group_by(distrito_id) |>
  summarise(electores = sum(mesa_electores, na.rm = TRUE))

# Unimos todo con las mismas columnas que la base 1983-2019
presi_2023 <- votos_2023 |>
  left_join(electores_2023, by = "distrito_id") |>
  left_join(codigos_distrito, by = "distrito_id") |>
  mutate(anio = 2023) |>
  select(anio, provincia, lista, tipo_voto, votos, electores)

# ------------------------------------------------------------------------------
# 5. Control con el total del país
# ------------------------------------------------------------------------------

# Referencia (primera vuelta 2023, sobre votos positivos, aproximado):
# Unión por la Patria ~37%, La Libertad Avanza ~30%, Juntos por el Cambio ~24%,
# Hacemos por Nuestro País ~7%, Frente de Izquierda ~3%
presi_2023 |>
  filter(tipo_voto == "positivo") |>
  group_by(lista) |>
  summarise(votos = sum(votos)) |>
  mutate(pct = round(votos / sum(votos) * 100, 1)) |>
  arrange(desc(votos))

# ------------------------------------------------------------------------------
# 6. Guardar
# ------------------------------------------------------------------------------

write_csv(presi_2023, "raw/presi_2023.csv")

# Liberamos memoria
rm(presi_mesas)
gc()
