# ------------------------------------------------------------------------------
# etiqueta_variable()
# Traduce el nombre interno de una variable al texto que se muestra en los
# gráficos. Ejemplo: "tercera_via_2019" -> "Tercera vía 2019"
# ------------------------------------------------------------------------------

etiqueta_variable <- function(variable) {
  espacios <- c(
    peronismo       = "Peronismo",
    jxc             = "JxC",
    derecha_liberal = "Derecha liberal",
    tercera_via     = "Tercera vía",
    izquierda       = "Izquierda"
  )
  codigo <- stringr::str_remove(variable, "_\\d{4}$")
  anio   <- stringr::str_extract(variable, "\\d{4}$")
  paste(unname(espacios[codigo]), anio)
}
