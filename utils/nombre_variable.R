# ------------------------------------------------------------------------------
# nombre_variable()
# Arma el nombre de una variable a partir del espacio y el año, en minúsculas,
# sin tildes ni espacios. Ejemplo: ("Tercera vía", 2019) -> "tercera_via_2019"
# ------------------------------------------------------------------------------

nombre_variable <- function(espacio, anio) {
  espacio |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("\\s+", "_") |>
    paste0("_", anio)
}
